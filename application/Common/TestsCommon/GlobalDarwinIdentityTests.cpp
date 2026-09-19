#include <QTest>

#include <Common/Global.h>
#include <SystemConfiguration/SystemConfiguration.h>
#include <cerrno>
#include <cstring>
#include <dlfcn.h>
#include <pwd.h>
#include <unistd.h>

namespace
{
   bool mockIdentity = false;
   QByteArray accountName;
   QString computerName;
   QByteArray hostname;
   int accountError = 0;
   bool missingAccount = false;
   bool hostnameError = false;
   bool unterminatedHostname = false;
   size_t requiredBufferSize = 0;
   int accountCalls = 0;
   int hostnameCalls = 0;
   uid_t queriedUid = 0;

   template<typename Function>
   Function native(const char* name)
   {
      return reinterpret_cast<Function>(dlsym(RTLD_NEXT, name));
   }
}

// Resolve native calls for the live-system smoke test; deterministic cases use
// executable-local symbols, leaving the production implementation unchanged.
extern "C" int getpwuid_r(uid_t uid, struct passwd* account, char* buffer, size_t size, struct passwd** result)
{
   if (!mockIdentity)
      return native<decltype(&getpwuid_r)>("getpwuid_r")(uid, account, buffer, size, result);
   ++accountCalls;
   queriedUid = uid;
   *result = nullptr;
   if (accountError)
      return accountError;
   if (missingAccount)
      return 0;
   if (size < requiredBufferSize || size <= size_t(accountName.size()))
      return ERANGE;
   std::memcpy(buffer, accountName.constData(), accountName.size() + 1);
   account->pw_name = buffer;
   *result = account;
   return 0;
}

extern "C" CFStringRef SCDynamicStoreCopyComputerName(SCDynamicStoreRef store, CFStringEncoding* encoding)
{
   if (!mockIdentity)
      return native<decltype(&SCDynamicStoreCopyComputerName)>("SCDynamicStoreCopyComputerName")(store, encoding);
   return computerName.isNull() ? nullptr : computerName.toCFString();
}

extern "C" int gethostname(char* buffer, size_t size)
{
   if (!mockIdentity)
      return native<decltype(&gethostname)>("gethostname")(buffer, size);
   ++hostnameCalls;
   if (hostnameError)
   {
      errno = EIO;
      return -1;
   }
   if (unterminatedHostname)
      std::memset(buffer, 'x', size);
   else
   {
      if (size <= size_t(hostname.size()))
         return -1;
      std::memcpy(buffer, hostname.constData(), hostname.size() + 1);
   }
   return 0;
}

class GlobalDarwinIdentityTests : public QObject
{
   Q_OBJECT

private slots:
   void init()
   {
      accountName = "test-account";
      computerName = QString::fromUtf8("Élodie’s Mac 日本語");
      hostname = "test-mac.local";
      accountError = 0;
      missingAccount = hostnameError = unterminatedHostname = false;
      requiredBufferSize = 0;
      accountCalls = hostnameCalls = 0;
      mockIdentity = true;
   }

   void cleanup() { mockIdentity = false; }

   void effectiveUser()
   {
      QCOMPARE(Common::Global::getCurrentUserName(), QString("test-account"));
      QCOMPARE(queriedUid, geteuid());
      QCOMPARE(accountCalls, 1);
   }

   void accountBufferGrowth()
   {
      requiredBufferSize = 16384;
      accountName = QString::fromUtf8("utilisateur-é").toUtf8();
      QCOMPARE(Common::Global::getCurrentUserName(), QString::fromUtf8(accountName));
      QVERIFY(accountCalls > 1);
   }

   void accountFailure_data()
   {
      QTest::addColumn<int>("error");
      QTest::newRow("unknown-account") << 0;
      QTest::newRow("lookup-error") << EIO;
      QTest::newRow("bounded-buffer-growth") << ERANGE;
   }

   void accountFailure()
   {
      QFETCH(int, error);
      missingAccount = error == 0;
      accountError = error;
      QVERIFY(Common::Global::getCurrentUserName().isEmpty());
      QVERIFY(accountCalls <= 11);
   }

   void configuredComputerName()
   {
      QCOMPARE(Common::Global::getCurrentMachineName(), computerName);
      QCOMPARE(hostnameCalls, 0);
      computerName = "Renamed Mac";
      QCOMPARE(Common::Global::getCurrentMachineName(), computerName); // No stale cached name.
   }

   void hostnameFallback_data()
   {
      QTest::addColumn<QString>("configuredName");
      QTest::newRow("unavailable") << QString();
      QTest::newRow("empty") << QStringLiteral("");
   }

   void hostnameFallback()
   {
      QFETCH(QString, configuredName);
      computerName = configuredName;
      QCOMPARE(Common::Global::getCurrentMachineName(), QString::fromUtf8(hostname));
      QCOMPARE(hostnameCalls, 1);
   }

   void hostnameFailureAndTruncation()
   {
      computerName = QString();
      hostnameError = true;
      QVERIFY(Common::Global::getCurrentMachineName().isEmpty());
      hostnameError = false;
      unterminatedHostname = true;
      QCOMPARE(Common::Global::getCurrentMachineName(), QString(255, 'x'));
   }

   void realSystemIdentity()
   {
      mockIdentity = false;
      // The single-threaded test can use getpwuid as an independent reference.
      const auto* account = getpwuid(geteuid());
      QVERIFY(account && account->pw_name);
      const QString expectedUser = QString::fromUtf8(account->pw_name);
      QCOMPARE(Common::Global::getCurrentUserName(), expectedUser);

      auto copyName = native<decltype(&SCDynamicStoreCopyComputerName)>("SCDynamicStoreCopyComputerName");
      QVERIFY(copyName);
      QString expectedComputer;
      if (const auto name = copyName(nullptr, nullptr))
      {
         expectedComputer = QString::fromCFString(name);
         CFRelease(name);
      }
      if (expectedComputer.isEmpty())
      {
         char buffer[256] = {};
         QCOMPARE(gethostname(buffer, sizeof(buffer)), 0);
         buffer[sizeof(buffer) - 1] = '\0';
         expectedComputer = QString::fromUtf8(buffer);
      }
      QVERIFY(!expectedComputer.isEmpty());
      QCOMPARE(Common::Global::getCurrentMachineName(), expectedComputer);
   }
};

QTEST_GUILESS_MAIN(GlobalDarwinIdentityTests)
#include "GlobalDarwinIdentityTests.moc"
