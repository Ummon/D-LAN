-module(d_lan_lang).
-export([langs/0, plain_lang/1, current_lang/1, tr/3, tr/4]).

-import(d_lan_common, [t/1]).

-include("/usr/lib/yaws/include/yaws_api.hrl").
-include("../include/d_lan_defines.hrl").

-spec langs() -> atom().
-spec plain_lang(atom()) -> string().
-spec current_lang(#arg{}) -> atom().
-spec tr(atom(), atom(), #arg{}) -> binary().
-spec tr(atom(), atom(), #arg{}, [term()]) -> binary().

% See here for the language codes : http://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
% Return a list of all accepted languages.
langs() ->
   [en, fr, de, it, ko].

plain_lang(en) -> t("English");
plain_lang(fr) -> t("Français");
plain_lang(de) -> t("Deutsch");
plain_lang(it) -> t("Italiano");
plain_lang(ko) -> t("한국어").

%%%%%%%%%%

translate(en, global, title) -> "D-LAN - A LAN file sharing software";
translate(fr, global, title) -> "D-LAN - Un logiciel de partage de fichiers en LAN";
translate(de, global, title) -> "D-LAN - Eine Software zum Dateiaustausch im LAN";
translate(it, global, title) -> "D-LAN - Un software di condivisione file in LAN";
translate(ko, global, title) -> "D-LAN - LAN 파일 공유 소프트웨어";

%%%%%%%%%%

translate(en, header, support_us) -> "support us!";
translate(fr, header, support_us) -> "soutenez-nous!";
translate(de, header, support_us) -> "unterstützen Sie uns!";
translate(it, header, support_us) -> "sosteneteci!";
translate(ko, header, support_us) -> "후원해 주세요!";

%%%%%%%%%%

translate(en, menu, home) -> "HOME";
translate(fr, menu, home) -> "HOME";
translate(de, menu, home) -> "STARTSEITE";
translate(it, menu, home) -> "HOME";
translate(ko, menu, home) -> "홈";

translate(en, menu, features) -> "FEATURES";
translate(fr, menu, features) -> "FONCTIONNALITÉS";
translate(de, menu, features) -> "FUNKTIONEN";
translate(it, menu, features) -> "FUNZIONALITÀ";
translate(ko, menu, features) -> "기능";

translate(en, menu, faq) -> "FAQ";
translate(fr, menu, faq) -> "FAQ";
translate(de, menu, faq) -> "FAQ";
translate(it, menu, faq) -> "FAQ";
translate(ko, menu, faq) -> "FAQ";

translate(en, menu, about) -> "ABOUT";
translate(fr, menu, about) -> "À PROPOS";
translate(de, menu, about) -> "ÜBER";
translate(it, menu, about) -> "INFORMAZIONI";
translate(ko, menu, about) -> "소개";

%%%%%%%%%%

translate(en, home, title) -> "D-LAN - A free <abbr title=\"Local Area Network\">LAN</abbr> file sharing software.";
translate(fr, home, title) -> "D-LAN - Un logiciel libre de partage de fichiers en <abbr title=\"Local Area Network (Réseau local)\">LAN</abbr>.";
translate(de, home, title) -> "D-LAN - Eine freie Software zum Dateiaustausch im <abbr title=\"Local Area Network (lokales Netzwerk)\">LAN</abbr>.";
translate(it, home, title) -> "D-LAN - Un software libero di condivisione file in <abbr title=\"Local Area Network (rete locale)\">LAN</abbr>.";
translate(ko, home, title) -> "D-LAN - 자유로운 <abbr title=\"Local Area Network (근거리 통신망)\">LAN</abbr> 파일 공유 소프트웨어.";

translate(en, home, description) -> "The goal is to easily exchange a large amount of data in a local area network environment like a LAN-Party. After you launched D-LAN, you will see all other people and their sharing automatically without special configuration. See the <a href=\"~s\">features list</a> for more information.";
translate(fr, home, description) -> "Le but est de permettre l'échange massif et facile de fichiers sur un réseau local, par exemple lors d'une LAN-Party. Après avoir lancé D-LAN, les autres personnes présentes sur le réseau sont visibles automatiquement sans aucune configuration particulière. Voir la <a href=\"~s\">page des fonctionnalités</a> pour plus d'informations.";
translate(de, home, description) -> "Das Ziel ist der einfache Austausch großer Datenmengen in einem lokalen Netzwerk, zum Beispiel auf einer LAN-Party. Nach dem Start von D-LAN werden alle anderen Teilnehmer und deren Freigaben automatisch angezeigt, ganz ohne besondere Konfiguration. Weitere Informationen finden Sie in der <a href=\"~s\">Funktionsliste</a>.";
translate(it, home, description) -> "L'obiettivo è scambiare facilmente grandi quantità di dati in una rete locale, ad esempio durante una LAN party. Dopo aver avviato D-LAN, tutte le altre persone e le loro condivisioni saranno visibili automaticamente, senza alcuna configurazione particolare. Per maggiori informazioni consultate l'<a href=\"~s\">elenco delle funzionalità</a>.";
translate(ko, home, description) -> "LAN 파티와 같은 근거리 네트워크 환경에서 대용량 데이터를 쉽게 교환하는 것이 목표입니다. D-LAN을 실행하면 별도의 설정 없이도 네트워크의 다른 모든 사용자와 그들의 공유 파일이 자동으로 표시됩니다. 자세한 내용은 <a href=\"~s\">기능 목록</a>을 참고하세요.";

translate(en, home, warning_beta) -> "<em>Warning:</em> The current version of D-LAN is a beta, it's only for test purpose. You can report any defect <a href=\"~s\">here</a>.";
translate(fr, home, warning_beta) -> "<em>Attention :</em> La version actuelle de D-LAN est une bêta, elle ne doit être utilisée qu'à des fins de tests. Il est possible de rapporter les anomalies rencontrées <a href=\"~s\">ici</a>.";
translate(de, home, warning_beta) -> "<em>Achtung:</em> Die aktuelle Version von D-LAN ist eine Beta und nur für Testzwecke gedacht. Fehler können <a href=\"~s\">hier</a> gemeldet werden.";
translate(it, home, warning_beta) -> "<em>Attenzione:</em> la versione attuale di D-LAN è una beta, da utilizzare solo a scopo di test. Potete segnalare eventuali difetti <a href=\"~s\">qui</a>.";
translate(ko, home, warning_beta) -> "<em>주의:</em> 현재 버전의 D-LAN은 베타 버전으로, 테스트 용도로만 사용해야 합니다. 발견한 결함은 <a href=\"~s\">여기</a>에서 신고할 수 있습니다.";

%%%%%%%%%%

translate(en, features, disclaimer) -> "<em>Here is the main features of the actual release.</em> D-LAN is constantly under development, you can see <a href=\"~s\">here</a> the planned features.";
translate(fr, features, disclaimer) -> "<em>Voici la liste des principales fonctionnalités de la version courante.</em> D-LAN est en développement constant, vous pouvez voir <a href=\"~s\">ici</a> les fonctionnalités planifiées pour les futures versions.";
translate(de, features, disclaimer) -> "<em>Hier sind die wichtigsten Funktionen der aktuellen Version.</em> D-LAN wird ständig weiterentwickelt, die geplanten Funktionen können <a href=\"~s\">hier</a> eingesehen werden.";
translate(it, features, disclaimer) -> "<em>Ecco le principali funzionalità della versione attuale.</em> D-LAN è in costante sviluppo, potete vedere <a href=\"~s\">qui</a> le funzionalità pianificate.";
translate(ko, features, disclaimer) -> "<em>다음은 현재 버전의 주요 기능입니다.</em> D-LAN은 지속적으로 개발되고 있으며, 계획된 기능은 <a href=\"~s\">여기</a>에서 확인할 수 있습니다.";

translate(en, features, feat_1) -> "Share files and folders in a local area network environment (LAN).";
translate(fr, features, feat_1) -> "Partage de fichiers et dossiers sur un réseau local (LAN).";
translate(de, features, feat_1) -> "Dateien und Ordner in einem lokalen Netzwerk (LAN) teilen.";
translate(it, features, feat_1) -> "Condivisione di file e cartelle in una rete locale (LAN).";
translate(ko, features, feat_1) -> "근거리 네트워크(LAN) 환경에서 파일과 폴더를 공유합니다.";

translate(en, features, feat_2) -> "Distributed transfers to increase performance and reliability.";
translate(fr, features, feat_2) -> "Transferts distribués pour de meilleures performances et une meilleure fiabilitée.";
translate(de, features, feat_2) -> "Verteilte Übertragungen für mehr Leistung und Zuverlässigkeit.";
translate(it, features, feat_2) -> "Trasferimenti distribuiti per migliorare prestazioni e affidabilità.";
translate(ko, features, feat_2) -> "분산 전송으로 성능과 안정성을 높입니다.";

translate(en, features, feat_3) -> "Very easy to use: no configuration, no central server.";
translate(fr, features, feat_3) -> "Très facile à utiliser, pas de configuration ni de serveur central.";
translate(de, features, feat_3) -> "Sehr einfach zu benutzen: keine Konfiguration, kein zentraler Server.";
translate(it, features, feat_3) -> "Molto facile da usare: nessuna configurazione, nessun server centrale.";
translate(ko, features, feat_3) -> "매우 쉬운 사용법: 별도의 설정도, 중앙 서버도 필요 없습니다.";

translate(en, features, feat_4) -> "Fast indexed search among all other peers.";
translate(fr, features, feat_4) -> "Recherche indexée rapide parmi l'ensemble des pairs.";
translate(de, features, feat_4) -> "Schnelle indizierte Suche über alle anderen Peers.";
translate(it, features, feat_4) -> "Ricerca indicizzata veloce tra tutti gli altri peer.";
translate(ko, features, feat_4) -> "모든 피어를 대상으로 한 빠른 색인 검색.";

translate(en, features, feat_5) -> "Browse all files and folders of any other peer.";
translate(fr, features, feat_5) -> "Possibilité de naviguer dans les fichiers et dossiers des autres pairs";
translate(de, features, feat_5) -> "Alle Dateien und Ordner jedes anderen Peers durchstöbern.";
translate(it, features, feat_5) -> "Possibilità di esplorare tutti i file e le cartelle degli altri peer.";
translate(ko, features, feat_5) -> "다른 피어의 모든 파일과 폴더를 탐색할 수 있습니다.";

translate(en, features, feat_6) -> "Manage the download queue. It includes adding, deleting or reordering.";
translate(fr, features, feat_6) -> "Gestion d'une liste des transferts. Il est possible d'ajouter, supprimer ou déplacer des transferts.";
translate(de, features, feat_6) -> "Verwaltung der Download-Warteschlange: Hinzufügen, Löschen und Umsortieren.";
translate(it, features, feat_6) -> "Gestione della coda di download: è possibile aggiungere, eliminare o riordinare i trasferimenti.";
translate(ko, features, feat_6) -> "다운로드 대기열 관리: 추가, 삭제, 순서 변경이 가능합니다.";

translate(en, features, feat_7) -> "Global chatting.";
translate(fr, features, feat_7) -> "Chat global";
translate(de, features, feat_7) -> "Globaler Chat.";
translate(it, features, feat_7) -> "Chat globale.";
translate(ko, features, feat_7) -> "전체 채팅.";

translate(en, features, feat_8) -> "D-LAN can run without graphic interface (GUI) and be controlled remotely.";
translate(fr, features, feat_8) -> "D-LAN peut être lancé sans interface graphique (<i>GUI</i>) et être piloté à distance.";
translate(de, features, feat_8) -> "D-LAN kann ohne grafische Oberfläche (GUI) laufen und ferngesteuert werden.";
translate(it, features, feat_8) -> "D-LAN può funzionare senza interfaccia grafica (GUI) ed essere controllato da remoto.";
translate(ko, features, feat_8) -> "D-LAN은 그래픽 인터페이스(GUI) 없이 실행할 수 있으며 원격으로 제어할 수 있습니다.";

translate(en, features, feat_9) -> "<a href=\"https://github.com/Ummon/D-LAN\">Open source</a>. Code source distributed under GPLv3 license.";
translate(fr, features, feat_9) -> "<a href=\"https://github.com/Ummon/D-LAN\">Open source</a>. Le code est distribué sous la licence GPLv3.";
translate(de, features, feat_9) -> "<a href=\"https://github.com/Ummon/D-LAN\">Open Source</a>. Der Quellcode wird unter der GPLv3-Lizenz vertrieben.";
translate(it, features, feat_9) -> "<a href=\"https://github.com/Ummon/D-LAN\">Open source</a>. Il codice sorgente è distribuito sotto licenza GPLv3.";
translate(ko, features, feat_9) -> "<a href=\"https://github.com/Ummon/D-LAN\">오픈 소스</a>. 소스 코드는 GPLv3 라이선스로 배포됩니다.";

translate(en, features, feat_10) -> "Free of any sort of ads or <a href=\"http://en.wikipedia.org/wiki/Malware\">malwares</a>.";
translate(fr, features, feat_10) -> "Ne contient aucune sorte de publicité ou de <a href=\"http://fr.wikipedia.org/wiki/Logiciel_malveillant\">'malware'</a>.";
translate(de, features, feat_10) -> "Frei von jeglicher Werbung und <a href=\"http://de.wikipedia.org/wiki/Schadprogramm\">Schadsoftware</a>.";
translate(it, features, feat_10) -> "Privo di qualsiasi pubblicità o <a href=\"http://it.wikipedia.org/wiki/Malware\">malware</a>.";
translate(ko, features, feat_10) -> "어떠한 광고나 <a href=\"http://ko.wikipedia.org/wiki/%EC%95%85%EC%84%B1_%EC%86%8C%ED%94%84%ED%8A%B8%EC%9B%A8%EC%96%B4\">악성 소프트웨어</a>도 포함하지 않습니다.";

translate(en, features, help_us) -> "Don't forget to <a href=\"~s\">support us</a>. It will help to maintain and add new features.";
translate(fr, features, help_us) -> "N'oubliez pas de nous <a href=\"~s\">aider</a>, cela permettra la maintenance et l'ajout de nouvelles fonctionnalités.";
translate(de, features, help_us) -> "Vergessen Sie nicht, uns zu <a href=\"~s\">unterstützen</a>. Das hilft bei der Wartung und der Entwicklung neuer Funktionen.";
translate(it, features, help_us) -> "Non dimenticate di <a href=\"~s\">sostenerci</a>: ciò aiuterà la manutenzione e l'aggiunta di nuove funzionalità.";
translate(ko, features, help_us) -> "<a href=\"~s\">후원</a>도 잊지 마세요. 유지 보수와 새로운 기능 추가에 도움이 됩니다.";


%%%%%%%%%%

translate(en, faq, q1) -> "What are the benefits of using D-LAN instead of the default system tools?";
translate(fr, faq, q1) -> "Quels sont les avantages d'utiliser D-LAN à la place des outils par défaut du système?";
translate(de, faq, q1) -> "Welche Vorteile bietet D-LAN gegenüber den Standardwerkzeugen des Systems?";
translate(it, faq, q1) -> "Quali sono i vantaggi di usare D-LAN rispetto agli strumenti predefiniti del sistema?";
translate(ko, faq, q1) -> "시스템 기본 도구 대신 D-LAN을 사용하면 어떤 이점이 있나요?";

translate(en, faq, a1) -> "D-LAN is designed for massive transfers, you can manage a queue of files to be downloaded. A file may be downloaded automatically from many peers at the same time to speed up the transfer and prevent peer downtime. D-LAN has a fast global search feature that the default system file sharing doesn't have. You will find more information from the <a href=\"features.html\">feature page</a>.";
translate(fr, faq, a1) -> "D-LAN est conçu pour des transfers massifs, il est possible de gérer une liste des fichiers à transferer. Un fichier peut être transferé depuis plusieurs pairs simultanément pour augmenter la vitesse ainsi que la fiabilité. Il est possible d'effectuer une recherche globale, ce que les partages système par défaut n'ont pas. Pour plus d'informations voir la <a href=\"features.html\">page des fonctionnalités</a>.";
translate(de, faq, a1) -> "D-LAN ist für massive Übertragungen ausgelegt: Es lässt sich eine Warteschlange der herunterzuladenden Dateien verwalten. Eine Datei kann automatisch von mehreren Peers gleichzeitig heruntergeladen werden, was die Übertragung beschleunigt und den Ausfall einzelner Peers ausgleicht. D-LAN bietet zudem eine schnelle globale Suche, die die Standard-Dateifreigabe des Systems nicht hat. Weitere Informationen finden Sie auf der <a href=\"features.html\">Funktionsseite</a>.";
translate(it, faq, a1) -> "D-LAN è progettato per trasferimenti massicci: è possibile gestire una coda di file da scaricare. Un file può essere scaricato automaticamente da più peer contemporaneamente, per accelerare il trasferimento e sopperire alla disconnessione di un peer. D-LAN dispone inoltre di una ricerca globale veloce, assente nella condivisione file predefinita del sistema. Trovate maggiori informazioni nella <a href=\"features.html\">pagina delle funzionalità</a>.";
translate(ko, faq, a1) -> "D-LAN은 대량 전송을 위해 설계되어 다운로드할 파일 대기열을 관리할 수 있습니다. 파일은 여러 피어로부터 동시에 자동으로 다운로드되어 전송 속도가 빨라지고 피어의 접속 종료에도 대비할 수 있습니다. 또한 시스템 기본 파일 공유에는 없는 빠른 전체 검색 기능이 있습니다. 자세한 내용은 <a href=\"features.html\">기능 페이지</a>를 참고하세요.";

translate(en, faq, q2) -> "I don't see other computers in my network.";
translate(fr, faq, q2) -> "Je ne vois pas les autres ordinateurs de mon réseau.";
translate(de, faq, q2) -> "Ich sehe die anderen Computer in meinem Netzwerk nicht.";
translate(it, faq, q2) -> "Non vedo gli altri computer della mia rete.";
translate(ko, faq, q2) -> "네트워크에서 다른 컴퓨터가 보이지 않습니다.";

translate(en, faq, a2) ->
   "<ul>"
   "<li>Check you have the latest version of D-LAN.</li>"
   "<li>Be sure the ports 59486 (<i>UDP</i>) and 59487 (<i>UDP + TCP</i>) are opened in your firewall.</li>"
   "<li>Be sure UDP multicast is allowed in your network. The address used is this one: 236.13.43.24.</li>"
   "<li>All peers must use the same protocol: IPv4 or IPv6, look on <i>Settings &gt; Network</i>.</li>"
   "</ul>";
translate(fr, faq, a2) ->
   "<ul>"
   "<li>Vérifier que la dernière version de D-LAN est installée.</li>"
   "<li>S'assurer que les ports 59486 (<i>UDP</i>) et 59487 (<i>UDP + TCP</i>) sont ouvert sur le pare-feu (<i>firewall</i>).</li>"
   "<li>S'assurer que l'UDP multicast est autorisé sur le réseau. L'adresse utilisée est la suivante : 236.13.43.24.</li>"
   "<li>Tous les pairs doivent utiliser le même protocole : IPv4 ou IPv6, voir <i>Paramètres &gt; Réseau</i>.</li>"
   "</ul>";
translate(de, faq, a2) ->
   "<ul>"
   "<li>Stellen Sie sicher, dass die neueste Version von D-LAN installiert ist.</li>"
   "<li>Stellen Sie sicher, dass die Ports 59486 (<i>UDP</i>) und 59487 (<i>UDP + TCP</i>) in Ihrer Firewall geöffnet sind.</li>"
   "<li>Stellen Sie sicher, dass UDP-Multicast in Ihrem Netzwerk erlaubt ist. Die verwendete Adresse lautet: 236.13.43.24.</li>"
   "<li>Alle Peers müssen dasselbe Protokoll verwenden: IPv4 oder IPv6, siehe <i>Einstellungen &gt; Netzwerk</i>.</li>"
   "</ul>";
translate(it, faq, a2) ->
   "<ul>"
   "<li>Verificate di avere l'ultima versione di D-LAN.</li>"
   "<li>Assicuratevi che le porte 59486 (<i>UDP</i>) e 59487 (<i>UDP + TCP</i>) siano aperte nel vostro firewall.</li>"
   "<li>Assicuratevi che il multicast UDP sia consentito nella vostra rete. L'indirizzo utilizzato è: 236.13.43.24.</li>"
   "<li>Tutti i peer devono usare lo stesso protocollo: IPv4 o IPv6, vedere <i>Impostazioni &gt; Rete</i>.</li>"
   "</ul>";
translate(ko, faq, a2) ->
   "<ul>"
   "<li>최신 버전의 D-LAN이 설치되어 있는지 확인하세요.</li>"
   "<li>방화벽에서 59486 (<i>UDP</i>) 및 59487 (<i>UDP + TCP</i>) 포트가 열려 있는지 확인하세요.</li>"
   "<li>네트워크에서 UDP 멀티캐스트가 허용되어 있는지 확인하세요. 사용되는 주소는 236.13.43.24입니다.</li>"
   "<li>모든 피어는 동일한 프로토콜(IPv4 또는 IPv6)을 사용해야 합니다. <i>설정 &gt; 네트워크</i>를 확인하세요.</li>"
   "</ul>";

translate(en, faq, q3) -> "D-LAN slows down my computer when hashing, what's that and why it's needed?";
translate(fr, faq, q3) -> "D-LAN ralentit mon ordinateur lors du calcul des empreintes, qu'est ce que c'est et pourquoi est-ce nécessaire?";
translate(de, faq, q3) -> "D-LAN verlangsamt meinen Computer beim Berechnen der Hashes. Was ist das und wozu ist es nötig?";
translate(it, faq, q3) -> "D-LAN rallenta il mio computer durante il calcolo degli hash: cosa sono e perché sono necessari?";
translate(ko, faq, q3) -> "해시 계산 중에 D-LAN이 컴퓨터를 느리게 합니다. 해시란 무엇이고 왜 필요한가요?";

translate(en, faq, a3) ->
   "hashes are necessary to identify all the parts of a file. It allows the multi-source downloading and the data integrity verification. During the hashing process only one core is used, almost all sold computer today are multi-core. All the sharing files has to be read once, so it can slow a bit the disk access.";
translate(fr, faq, a3) ->
   "Les empreintes servent à identifier les données des fichiers. Cela permet de télécharger un fichier chez plusieurs pairs simultanément. "
   "Le calcul des empreintes n'utilise pas plus d'un coeur du processeur (la plus part des processeurs actuels sont multi-coeurs) "
   "mais doit lire toutes les données partagées ce qui peut ralentir un peu l'accès au disque dur.";
translate(de, faq, a3) ->
   "Hashes sind nötig, um alle Teile einer Datei zu identifizieren. Sie ermöglichen das Herunterladen aus mehreren Quellen sowie die Überprüfung der Datenintegrität. "
   "Während der Berechnung wird nur ein Prozessorkern verwendet, und fast alle heute verkauften Computer haben mehrere Kerne. "
   "Alle freigegebenen Dateien müssen einmal gelesen werden, was den Festplattenzugriff etwas verlangsamen kann.";
translate(it, faq, a3) ->
   "Gli hash sono necessari per identificare tutte le parti di un file. Permettono il download da più fonti e la verifica dell'integrità dei dati. "
   "Durante il calcolo degli hash viene usato un solo core, e quasi tutti i computer venduti oggi sono multi-core. "
   "Tutti i file condivisi devono essere letti una volta, il che può rallentare un po' l'accesso al disco.";
translate(ko, faq, a3) ->
   "해시는 파일의 각 부분을 식별하는 데 필요합니다. 이를 통해 다중 소스 다운로드와 데이터 무결성 검증이 가능합니다. "
   "해시 계산에는 코어 하나만 사용되며, 요즘 판매되는 컴퓨터는 대부분 멀티 코어입니다. "
   "공유되는 모든 파일을 한 번씩 읽어야 하므로 디스크 접근이 다소 느려질 수 있습니다.";

translate(en, faq, q4) ->
   "D-LAN uses too much ressources, how can I improve that?";
translate(fr, faq, q4) ->
   "D-LAN utilise trop de ressource, comment puis-je faire pour réduire sa consommation?";
translate(de, faq, q4) ->
   "D-LAN verbraucht zu viele Ressourcen, was kann ich dagegen tun?";
translate(it, faq, q4) ->
   "D-LAN usa troppe risorse, come posso migliorare la situazione?";
translate(ko, faq, q4) ->
   "D-LAN이 리소스를 너무 많이 사용합니다. 어떻게 개선할 수 있나요?";

translate(en, faq, a4) ->
   "<p>If D-LAN is currently computing some hashes, you have to wait the end of this operation, see the previous question. Otherwise it's possible to close the main D-LAN window, the GUI will be disconnected from the core and take less ressources.</p><p>Try to not have more than 10'000 files in queue, periodically clear the queue.</p>";
translate(fr, faq, a4) ->
   "<p>Si D-LAN est en train de calculer les empreintes, il faut attendre que l'opération soit terminée, voir la question précédente. "
   "Sinon il est possible de fermer la fenêtre de D-LAN, l'interface est alors déconnectée du coeur et consomme moins de ressources.</p><p>Éviter d'avoir plus de 10'000 fichiers en queue, périodiquement enlever les fichiers complets.</p>";
translate(de, faq, a4) ->
   "<p>Wenn D-LAN gerade Hashes berechnet, müssen Sie das Ende dieses Vorgangs abwarten, siehe die vorherige Frage. "
   "Ansonsten kann das Hauptfenster von D-LAN geschlossen werden: Die Oberfläche wird dann vom Kern getrennt und verbraucht weniger Ressourcen.</p><p>Vermeiden Sie mehr als 10'000 Dateien in der Warteschlange und leeren Sie diese regelmäßig.</p>";
translate(it, faq, a4) ->
   "<p>Se D-LAN sta calcolando degli hash, bisogna attendere la fine dell'operazione, vedere la domanda precedente. "
   "Altrimenti è possibile chiudere la finestra principale di D-LAN: l'interfaccia verrà disconnessa dal core e userà meno risorse.</p><p>Cercate di non avere più di 10'000 file in coda e svuotatela periodicamente.</p>";
translate(ko, faq, a4) ->
   "<p>D-LAN이 해시를 계산 중이라면 이 작업이 끝날 때까지 기다려야 합니다(이전 질문 참고). "
   "그 외의 경우 D-LAN 메인 창을 닫으면 GUI가 코어에서 분리되어 리소스를 덜 사용합니다.</p><p>대기열에 10,000개 이상의 파일을 두지 않도록 하고, 주기적으로 대기열을 정리하세요.</p>";

translate(en, faq, q5) -> "There is no Mac OS X version!?";
translate(fr, faq, q5) -> "Il n'y a pas de version pour Mac OS X!?";
translate(de, faq, q5) -> "Es gibt keine Version für Mac OS X!?";
translate(it, faq, q5) -> "Non c'è una versione per Mac OS X!?";
translate(ko, faq, q5) -> "Mac OS X 버전이 없나요!?";

translate(en, faq, a5) -> "We are currently working on a Mac OS X version, it will be released when it's done.";
translate(fr, faq, a5) -> "Nous travaillons actuellement sur une version Mac OS X, elle sortira quand elle sera prête.";
translate(de, faq, a5) -> "Wir arbeiten derzeit an einer Version für Mac OS X, sie wird veröffentlicht, sobald sie fertig ist.";
translate(it, faq, a5) -> "Stiamo attualmente lavorando a una versione per Mac OS X, sarà pubblicata quando sarà pronta.";
translate(ko, faq, a5) -> "현재 Mac OS X 버전을 개발 중이며, 완성되면 공개될 예정입니다.";

translate(en, faq, q6) -> "Can I configure D-LAN to start automatically when my computer starting?";
translate(fr, faq, q6) -> "Est-il possible de configurer D-LAN pour qu'il démarre automatiquement au démarrage de la machine?";
translate(de, faq, q6) -> "Kann D-LAN so konfiguriert werden, dass es beim Hochfahren des Computers automatisch startet?";
translate(it, faq, q6) -> "Posso configurare D-LAN perché si avvii automaticamente all'accensione del computer?";
translate(ko, faq, q6) -> "컴퓨터가 시작될 때 D-LAN이 자동으로 실행되도록 설정할 수 있나요?";

translate(en, faq, a6) -> "<i>Windows 7</i> : Go to <i>Control Panel</i> &gt; <i>Administrative Tools</i> &gt; <i>Services</i> . Open the properties of <i>D-LAN Core</i> and set the <i>Startup type</i> from <i>Manual</i> to <i>Automatic</i>.";
translate(fr, faq, a6) -> "<i>Windows 7</i> : Allez dans <i>Panneau de configuration</i> > <i>Outils d'administrations</i> &gt; <i>Services</i>. Ouvrir la fenêtre de propriétés de <i>D-LAN Core</i> et définir le <i>Type de Démarrage</i> à <i>Automatique</i>.";
translate(de, faq, a6) -> "<i>Windows 7</i>: Gehen Sie zu <i>Systemsteuerung</i> &gt; <i>Verwaltung</i> &gt; <i>Dienste</i>. Öffnen Sie die Eigenschaften von <i>D-LAN Core</i> und stellen Sie den <i>Starttyp</i> von <i>Manuell</i> auf <i>Automatisch</i>.";
translate(it, faq, a6) -> "<i>Windows 7</i>: andate in <i>Pannello di controllo</i> &gt; <i>Strumenti di amministrazione</i> &gt; <i>Servizi</i>. Aprite le proprietà di <i>D-LAN Core</i> e impostate il <i>Tipo di avvio</i> da <i>Manuale</i> ad <i>Automatico</i>.";
translate(ko, faq, a6) -> "<i>Windows 7</i> : <i>제어판</i> &gt; <i>관리 도구</i> &gt; <i>서비스</i>로 이동합니다. <i>D-LAN Core</i>의 속성을 열고 <i>시작 유형</i>을 <i>수동</i>에서 <i>자동</i>으로 변경하세요.";

%%%%%%%%%%

translate(en, about, author) -> "Author : ~s";
translate(fr, about, author) -> "Auteur : ~s";
translate(de, about, author) -> "Autor: ~s";
translate(it, about, author) -> "Autore: ~s";
translate(ko, about, author) -> "제작자 : ~s";

translate(en, about, linux) -> "Linux maintainer : ~s";
translate(fr, about, linux) -> "Responsable Linux : ~s";
translate(de, about, linux) -> "Linux-Betreuer: ~s";
translate(it, about, linux) -> "Responsabile Linux: ~s";
translate(ko, about, linux) -> "Linux 담당자 : ~s";

translate(en, about, thanks) -> "Thanks to ~s and ~s for their support.";
translate(fr, about, thanks) -> "Merci à ~s et ~s pour leur support.";
translate(de, about, thanks) -> "Dank an ~s und ~s für ihre Unterstützung.";
translate(it, about, thanks) -> "Grazie a ~s e ~s per il loro supporto.";
translate(ko, about, thanks) -> "지원해 주신 ~s님과 ~s님께 감사드립니다.";

translate(en, about, tech) -> "Technologies and softwares used";
translate(fr, about, tech) -> "Technologies et logiciels utilisés";
translate(de, about, tech) -> "Verwendete Technologien und Software";
translate(it, about, tech) -> "Tecnologie e software utilizzati";
translate(ko, about, tech) -> "사용된 기술 및 소프트웨어";

translate(en, about, tech_used_d_lan_title) -> "D-LAN";
translate(fr, about, tech_used_d_lan_title) -> "D-LAN";
translate(de, about, tech_used_d_lan_title) -> "D-LAN";
translate(it, about, tech_used_d_lan_title) -> "D-LAN";
translate(ko, about, tech_used_d_lan_title) -> "D-LAN";

translate(en, about, tech_used_d_lan) ->
   "<li>Programming language: <a href=\"http://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
   "<li>Framework and libraries: <a href=\"https://www.qt.io/development/qt-framework\">Qt 6</a></li>"
   "<li>Compiler: <a href=\"https://clang.llvm.org/\">Clang</a></li>"
   "<li>Message serializer: <a href=\"http://code.google.com/p/protobuf\">Protocol Buffers</a></li>"
   "<li>Cryptographic hash function: <a href=\"https://github.com/BLAKE3-team/BLAKE3\">BLAKE3</a></li>"
   "<li>Database: <a href=\"https://www.sqlite.org/\">SQLite</a></li>";
translate(fr, about, tech_used_d_lan) ->
   "<li>Language de programmation : <a href=\"http://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
   "<li>Bibliothèque logicielle principale  : <a href=\"https://www.qt.io/development/qt-framework\">Qt 6</a></li>"
   "<li>Compilateur : <a href=\"https://clang.llvm.org/\">Clang</a></li>"
   "<li>Sérialisation des messages : <a href=\"http://code.google.com/p/protobuf\">Protocol Buffers</a></li>"
   "<li>Fonction de hashage : <a href=\"https://github.com/BLAKE3-team/BLAKE3\">BLAKE3</a></li>"
   "<li>Base de données : <a href=\"https://www.sqlite.org/\">SQLite</a></li>";
translate(de, about, tech_used_d_lan) ->
   "<li>Programmiersprache: <a href=\"http://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
   "<li>Framework und Bibliotheken: <a href=\"https://www.qt.io/development/qt-framework\">Qt 6</a></li>"
   "<li>Compiler: <a href=\"https://clang.llvm.org/\">Clang</a></li>"
   "<li>Nachrichten-Serialisierung: <a href=\"http://code.google.com/p/protobuf\">Protocol Buffers</a></li>"
   "<li>Kryptografische Hashfunktion: <a href=\"https://github.com/BLAKE3-team/BLAKE3\">BLAKE3</a></li>"
   "<li>Datenbank: <a href=\"https://www.sqlite.org/\">SQLite</a></li>";
translate(it, about, tech_used_d_lan) ->
   "<li>Linguaggio di programmazione: <a href=\"http://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
   "<li>Framework e librerie: <a href=\"https://www.qt.io/development/qt-framework\">Qt 6</a></li>"
   "<li>Compilatore: <a href=\"https://clang.llvm.org/\">Clang</a></li>"
   "<li>Serializzazione dei messaggi: <a href=\"http://code.google.com/p/protobuf\">Protocol Buffers</a></li>"
   "<li>Funzione di hash crittografica: <a href=\"https://github.com/BLAKE3-team/BLAKE3\">BLAKE3</a></li>"
   "<li>Database: <a href=\"https://www.sqlite.org/\">SQLite</a></li>";
translate(ko, about, tech_used_d_lan) ->
   "<li>프로그래밍 언어 : <a href=\"http://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
   "<li>프레임워크 및 라이브러리 : <a href=\"https://www.qt.io/development/qt-framework\">Qt 6</a></li>"
   "<li>컴파일러 : <a href=\"https://clang.llvm.org/\">Clang</a></li>"
   "<li>메시지 직렬화 : <a href=\"http://code.google.com/p/protobuf\">Protocol Buffers</a></li>"
   "<li>암호화 해시 함수 : <a href=\"https://github.com/BLAKE3-team/BLAKE3\">BLAKE3</a></li>"
   "<li>데이터베이스 : <a href=\"https://www.sqlite.org/\">SQLite</a></li>";


translate(en, about, tech_used_tools_title) -> "Developpment tools";
translate(fr, about, tech_used_tools_title) -> "Outils de développement";
translate(de, about, tech_used_tools_title) -> "Entwicklungswerkzeuge";
translate(it, about, tech_used_tools_title) -> "Strumenti di sviluppo";
translate(ko, about, tech_used_tools_title) -> "개발 도구";

translate(en, about, tech_used_tools) ->
   "<li>Development environment: <a href=\"http://qt.nokia.com/products/developer-tools/\">Qt Creator</a></li>"
   "<li>Version control system: <a href=\"http://git-scm.com/\">git</li>"
   "<li>Scripting: <a href=\"https://www.nushell.sh/\">Nushell</a></li>"
   "<li>Project management: <a href=\"http://www.redmine.org/\">Redmine</a></li>"
   "<li>Documentation generator: <a href=\"http://www.doxygen.org/\">Doxygen</a></li>"
   "<li>Setup builder: <a href=\"http://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
   "<li>Vector and bitmap graphics editor: <a href=\"https://www.affinity.studio/\">Affinity</a></li>";
translate(fr, about, tech_used_tools) ->
   "<li>Environnement de développement : <a href=\"http://qt.nokia.com/products/developer-tools/\">Qt Creator</a></li>"
   "<li>Système de gestion de versions : <a href=\"http://git-scm.com/\">git</a></li>"
   "<li>Scripting : <a href=\"https://www.nushell.sh/\">Nushell</a></li>"
   "<li>Gestion de projet : <a href=\"http://www.redmine.org/\">Redmine</a></li>"
   "<li>Génération de la documentation : <a href=\"http://www.doxygen.org/\">Doxygen</a></li>"
   "<li>Système d'installation : <a href=\"http://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
   "<li>Éditeur vectoriel et bitmap : <a href=\"https://www.affinity.studio/\">Affinity</a></li>";
translate(de, about, tech_used_tools) ->
   "<li>Entwicklungsumgebung: <a href=\"http://qt.nokia.com/products/developer-tools/\">Qt Creator</a></li>"
   "<li>Versionsverwaltung: <a href=\"http://git-scm.com/\">git</a></li>"
   "<li>Skripting: <a href=\"https://www.nushell.sh/\">Nushell</a></li>"
   "<li>Projektverwaltung: <a href=\"http://www.redmine.org/\">Redmine</a></li>"
   "<li>Dokumentationsgenerator: <a href=\"http://www.doxygen.org/\">Doxygen</a></li>"
   "<li>Installationsprogramm: <a href=\"http://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
   "<li>Vektor- und Bitmap-Grafikeditor: <a href=\"https://www.affinity.studio/\">Affinity</a></li>";
translate(it, about, tech_used_tools) ->
   "<li>Ambiente di sviluppo: <a href=\"http://qt.nokia.com/products/developer-tools/\">Qt Creator</a></li>"
   "<li>Sistema di controllo versione: <a href=\"http://git-scm.com/\">git</a></li>"
   "<li>Scripting: <a href=\"https://www.nushell.sh/\">Nushell</a></li>"
   "<li>Gestione del progetto: <a href=\"http://www.redmine.org/\">Redmine</a></li>"
   "<li>Generatore di documentazione: <a href=\"http://www.doxygen.org/\">Doxygen</a></li>"
   "<li>Creazione dell'installer: <a href=\"http://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
   "<li>Editor di grafica vettoriale e bitmap: <a href=\"https://www.affinity.studio/\">Affinity</a></li>";
translate(ko, about, tech_used_tools) ->
   "<li>개발 환경 : <a href=\"http://qt.nokia.com/products/developer-tools/\">Qt Creator</a></li>"
   "<li>버전 관리 시스템 : <a href=\"http://git-scm.com/\">git</a></li>"
   "<li>스크립팅 : <a href=\"https://www.nushell.sh/\">Nushell</a></li>"
   "<li>프로젝트 관리 : <a href=\"http://www.redmine.org/\">Redmine</a></li>"
   "<li>문서 생성기 : <a href=\"http://www.doxygen.org/\">Doxygen</a></li>"
   "<li>설치 프로그램 제작 : <a href=\"http://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
   "<li>벡터 및 비트맵 그래픽 편집기 : <a href=\"https://www.affinity.studio/\">Affinity</a></li>";

translate(en, about, tech_used_website_title) -> "Web site";
translate(fr, about, tech_used_website_title) -> "Site web";
translate(de, about, tech_used_website_title) -> "Webseite";
translate(it, about, tech_used_website_title) -> "Sito web";
translate(ko, about, tech_used_website_title) -> "웹 사이트";

translate(en, about, tech_used_website) ->
   "<li>Document structure: <a href=\"http://www.w3.org/TR/html5/\">HTML5</a></li>"
   "<li>Document presentation: <a href=\"http://www.w3.org/Style/CSS/current-work\">CSS 3</a> + <a href=\"http://sass-lang.com\">Sass</a></li>"
   "<li>Client side dynamic language: <a href=\"http://fr.wikipedia.org/wiki/JavaScript\">JavaScript</a></li>"
   "<li>JavaScript libraries: <a href=\"http://jquery.com/\">JQuery</a> + <a href=\"http://colorpowered.com/colorbox/\">ColorBox</a></li>"
   "<li>Server side language: <a href=\"http://www.erlang.org/\">Erlang</a></li>"
   "<li>Web server: <a href=\"http://yaws.hyber.org/\">Yaws</a></li>";
translate(fr, about, tech_used_website) ->
   "<li>Structure : <a href=\"http://dev.w3.org/html5/spec/Overview.html\">HTML 5</a></li>"
   "<li>Présentation : <a href=\"http://www.w3.org/TR/css3-roadmap/\">CSS3</a> + <a href=\"http://sass-lang.com\">Sass</a></li>"
   "<li>Langage dynamque coté client : <a href=\"http://fr.wikipedia.org/wiki/JavaScript\">JavaScript</a></li>"
   "<li>Bibliothèques JavaScript : <a href=\"http://jquery.com/\">JQuery</a> + <a href=\"http://colorpowered.com/colorbox/\">ColorBox</a></li>"
   "<li>Langage coté serveur : <a href=\"http://www.erlang.org/\">Erlang</a></li>"
   "<li>Serveur web : <a href=\"http://yaws.hyber.org/\">Yaws</a></li>";
translate(de, about, tech_used_website) ->
   "<li>Dokumentstruktur: <a href=\"http://www.w3.org/TR/html5/\">HTML5</a></li>"
   "<li>Dokumentdarstellung: <a href=\"http://www.w3.org/Style/CSS/current-work\">CSS 3</a> + <a href=\"http://sass-lang.com\">Sass</a></li>"
   "<li>Clientseitige dynamische Sprache: <a href=\"http://de.wikipedia.org/wiki/JavaScript\">JavaScript</a></li>"
   "<li>JavaScript-Bibliotheken: <a href=\"http://jquery.com/\">JQuery</a> + <a href=\"http://colorpowered.com/colorbox/\">ColorBox</a></li>"
   "<li>Serverseitige Sprache: <a href=\"http://www.erlang.org/\">Erlang</a></li>"
   "<li>Webserver: <a href=\"http://yaws.hyber.org/\">Yaws</a></li>";
translate(it, about, tech_used_website) ->
   "<li>Struttura del documento: <a href=\"http://www.w3.org/TR/html5/\">HTML5</a></li>"
   "<li>Presentazione del documento: <a href=\"http://www.w3.org/Style/CSS/current-work\">CSS 3</a> + <a href=\"http://sass-lang.com\">Sass</a></li>"
   "<li>Linguaggio dinamico lato client: <a href=\"http://it.wikipedia.org/wiki/JavaScript\">JavaScript</a></li>"
   "<li>Librerie JavaScript: <a href=\"http://jquery.com/\">JQuery</a> + <a href=\"http://colorpowered.com/colorbox/\">ColorBox</a></li>"
   "<li>Linguaggio lato server: <a href=\"http://www.erlang.org/\">Erlang</a></li>"
   "<li>Server web: <a href=\"http://yaws.hyber.org/\">Yaws</a></li>";
translate(ko, about, tech_used_website) ->
   "<li>문서 구조 : <a href=\"http://www.w3.org/TR/html5/\">HTML5</a></li>"
   "<li>문서 표현 : <a href=\"http://www.w3.org/Style/CSS/current-work\">CSS 3</a> + <a href=\"http://sass-lang.com\">Sass</a></li>"
   "<li>클라이언트 측 동적 언어 : <a href=\"http://ko.wikipedia.org/wiki/자바스크립트\">JavaScript</a></li>"
   "<li>JavaScript 라이브러리 : <a href=\"http://jquery.com/\">JQuery</a> + <a href=\"http://colorpowered.com/colorbox/\">ColorBox</a></li>"
   "<li>서버 측 언어 : <a href=\"http://www.erlang.org/\">Erlang</a></li>"
   "<li>웹 서버 : <a href=\"http://yaws.hyber.org/\">Yaws</a></li>";

%%%%%%%%%%

translate(en, donate, title) -> "Support us";
translate(fr, donate, title) -> "Soutenez-nous";
translate(de, donate, title) -> "Unterstützen Sie uns";
translate(it, donate, title) -> "Sosteneteci";
translate(ko, donate, title) -> "후원하기";

translate(en, donate, intro) -> "If you like this project and want to see it grow, support us!";
translate(fr, donate, intro) -> "Si vous aimez ce projet et que vous voulez le voir grandir, soutenez-nous !";
translate(de, donate, intro) -> "Wenn Ihnen dieses Projekt gefällt und Sie es wachsen sehen möchten, unterstützen Sie uns!";
translate(it, donate, intro) -> "Se vi piace questo progetto e volete vederlo crescere, sosteneteci!";
translate(ko, donate, intro) -> "이 프로젝트가 마음에 들고 성장하는 모습을 보고 싶다면 후원해 주세요!";

translate(en, donate, bitcoin_address) -> "Bitcoin address: ";
translate(fr, donate, bitcoin_address) -> "Adresse bitcoin : ";
translate(de, donate, bitcoin_address) -> "Bitcoin-Adresse: ";
translate(it, donate, bitcoin_address) -> "Indirizzo bitcoin: ";
translate(ko, donate, bitcoin_address) -> "비트코인 주소 : ";

%%%%%%%%%%

translate(en, gallery, browse) -> "Browsing";
translate(fr, gallery, browse) -> "Navigation";
translate(de, gallery, browse) -> "Durchstöbern";
translate(it, gallery, browse) -> "Esplorazione";
translate(ko, gallery, browse) -> "탐색";

translate(en, gallery, browse_comment) -> "Browsing files and folders of a peer";
translate(fr, gallery, browse_comment) -> "Navigation dans les fichiers et dossiers d'un pair";
translate(de, gallery, browse_comment) -> "Durchstöbern der Dateien und Ordner eines Peers";
translate(it, gallery, browse_comment) -> "Esplorazione dei file e delle cartelle di un peer";
translate(ko, gallery, browse_comment) -> "피어의 파일과 폴더 탐색";

translate(en, gallery, search) -> "Search result";
translate(fr, gallery, search) -> "Résultat de la recherche";
translate(de, gallery, search) -> "Suchergebnis";
translate(it, gallery, search) -> "Risultato della ricerca";
translate(ko, gallery, search) -> "검색 결과";

translate(en, gallery, search_comment) -> "The results are sorted by relevance. Folders are put on top.";
translate(fr, gallery, search_comment) -> "Les résultats sont triés par pertinence. Les dossier sont placés en premiers.";
translate(de, gallery, search_comment) -> "Die Ergebnisse sind nach Relevanz sortiert. Ordner stehen oben.";
translate(it, gallery, search_comment) -> "I risultati sono ordinati per rilevanza. Le cartelle sono mostrate in alto.";
translate(ko, gallery, search_comment) -> "결과는 관련도 순으로 정렬되며, 폴더가 위에 표시됩니다.";

translate(en, gallery, download_folders) -> "Downloads - Folders";
translate(fr, gallery, download_folders) -> "Transferts - Dossier";
translate(de, gallery, download_folders) -> "Downloads - Ordner";
translate(it, gallery, download_folders) -> "Download - Cartelle";
translate(ko, gallery, download_folders) -> "다운로드 - 폴더";

translate(en, gallery, download_folders_comment) -> "This view shows the files with their folders, they are both sorted alphabetically.";
translate(fr, gallery, download_folders_comment) -> "Cette vue montre les fichiers avec leurs dossiers, ils sont triés alphabétiquement.";
translate(de, gallery, download_folders_comment) -> "Diese Ansicht zeigt die Dateien mit ihren Ordnern, beide alphabetisch sortiert.";
translate(it, gallery, download_folders_comment) -> "Questa vista mostra i file con le loro cartelle, entrambi in ordine alfabetico.";
translate(ko, gallery, download_folders_comment) -> "이 화면은 파일을 폴더와 함께 보여 주며, 모두 이름순으로 정렬됩니다.";

translate(en, gallery, download_files) -> "Downloads - Files";
translate(fr, gallery, download_files) -> "Transferts - Fichiers";
translate(de, gallery, download_files) -> "Downloads - Dateien";
translate(it, gallery, download_files) -> "Download - File";
translate(ko, gallery, download_files) -> "다운로드 - 파일";

translate(en, gallery, download_files_comment) -> "This view shows only the files, they can be rearranged, the top files are downloaded first.";
translate(fr, gallery, download_files_comment) -> "Cette vue montre seulement les fichiers, ils peuvent être réordonnés, les fichiers en haut sont téléchargés en premiers.";
translate(de, gallery, download_files_comment) -> "Diese Ansicht zeigt nur die Dateien; sie können umsortiert werden, die obersten Dateien werden zuerst heruntergeladen.";
translate(it, gallery, download_files_comment) -> "Questa vista mostra solo i file; possono essere riordinati, i file in alto vengono scaricati per primi.";
translate(ko, gallery, download_files_comment) -> "이 화면은 파일만 보여 주며, 순서를 변경할 수 있고 위쪽 파일이 먼저 다운로드됩니다.";

translate(en, gallery, upload) -> "Upload view";
translate(fr, gallery, upload) -> "Vue des envoies";
translate(de, gallery, upload) -> "Upload-Ansicht";
translate(it, gallery, upload) -> "Vista degli upload";
translate(ko, gallery, upload) -> "업로드 화면";

translate(en, gallery, skin) -> "Skin";
translate(fr, gallery, skin) -> "Skin";
translate(de, gallery, skin) -> "Skin";
translate(it, gallery, skin) -> "Skin";
translate(ko, gallery, skin) -> "스킨";

%%%%%%%%%%

translate(en, download_button, download) -> "Download D-LAN";
translate(fr, download_button, download) -> "Télécharger D-LAN";
translate(de, download_button, download) -> "D-LAN herunterladen";
translate(it, download_button, download) -> "Scarica D-LAN";
translate(ko, download_button, download) -> "D-LAN 다운로드";

translate(en, download_button, version) -> "Version ~s for ~s";
translate(fr, download_button, version) -> "Version ~s pour ~s";
translate(de, download_button, version) -> "Version ~s für ~s";
translate(it, download_button, version) -> "Versione ~s per ~s";
translate(ko, download_button, version) -> "버전 ~s (~s용)";

translate(en, download_button, released) -> "Released on ~s";
translate(fr, download_button, released) -> "Sorti le ~s";
translate(de, download_button, released) -> "Veröffentlicht am ~s";
translate(it, download_button, released) -> "Pubblicato il ~s";
translate(ko, download_button, released) -> "출시일 : ~s";

translate(en, download_button, torrent) -> "Download with BitTorrent";
translate(fr, download_button, torrent) -> "Télécharger avec BitTorrent";
translate(de, download_button, torrent) -> "Mit BitTorrent herunterladen";
translate(it, download_button, torrent) -> "Scarica con BitTorrent";
translate(ko, download_button, torrent) -> "BitTorrent로 다운로드";

translate(en, _, _) -> "No translation.";
translate(fr, _, _) -> "Pas de traduction";
translate(de, _, _) -> "Keine Übersetzung.";
translate(it, _, _) -> "Nessuna traduzione.";
translate(ko, _, _) -> "번역이 없습니다.";

translate(_, _, _) -> "<<translation mising>>".

%%%%%%%%%%

% Return the current language depending the 'lang' GET ou the cookies.
% Contained in 'langs()'.
current_lang(A) ->
   % 1) Looks if a GET variable 'lang' is defined
   case yaws_api:queryvar(A, "lang") of
      {ok, L} -> list_to_lang(L);
      _ ->
         % 2) Looks if a 'lang' value exist in a cookie.
         case yaws_api:find_cookie_val("lang", (A#arg.headers)#headers.cookie) of
         [] ->
            % 3) Looks in the "Accept-Language" HTTP header field.
            case accepted_langs_by_user_agent(A) of
               [Lang | _] -> Lang;
               _ -> hd(langs())
            end;
         C -> list_to_lang(C)
      end
   end.

% Returns a known atom language from a string().
% Returns the first known language if the given string is unknown.
% See 'langs()'.
list_to_lang(Lang_str) ->
   try list_to_existing_atom(Lang_str) of
      Lang ->
         case lists:member(Lang, langs()) of
            true -> Lang;
            _ -> hd(langs()) % The language isn't defined (or known) we take the first of the list.
         end
   catch
      error:_ -> hd(langs())
   end.

% Return a list of accepted languages by the user agent. Return only known languages from 'langs/0'.
% Read the HTTP field 'Accept-Language'.
accepted_langs_by_user_agent(A) ->
   lists:map(
      fun ({Lang, _}) -> Lang end, % Remove the quality information.
      lists:reverse(lists:keysort(2, % Sort by quality, bigger first.
         case lists:filter(fun({http_header, _, Name, _, _}) -> Name =:= 'Accept-Language' end, (A#arg.headers)#headers.other) of
            [{http_header, _, _, _, Values} | _] ->
               % erlang:display(Values), % For debug purpose.
               lists:foldr(
                  fun(Val, Acc) ->
                     {Lang_str_with_subtag, Quality} = case string:tokens(string:strip(Val), ";") of
                        [L, "q=" ++ Q] -> {L, list_to_float(Q)};
                        [L | _] -> {L, 1.0}
                     end,
                     [Lang_str | _] = string:tokens(Lang_str_with_subtag, "-"), % We don't care about the subtags.
                     try list_to_existing_atom(Lang_str) of
                        Lang ->
                           case lists:member(Lang, langs()) of
                              true -> [{Lang, Quality} | Acc]; % We keep only known languages.
                              _ -> Acc
                           end
                     catch
                        error:_ -> Acc
                     end
                  end,
                  [],
                  string:tokens(Values, ",")
               );
            _ -> []
         end
      ))
   ).

tr(Page, Section, A) ->
   tr(Page, Section, A, []).

tr(Page, Section, A, Params) ->
   t(io_lib:format(translate(current_lang(A), Page, Section), Params)).
