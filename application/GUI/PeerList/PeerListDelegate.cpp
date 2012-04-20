#include <PeerList/PeerListDelegate.h>
using namespace GUI;

#include <QPainter>

#include <Common/Settings.h>

#include <PeerList/PeerListModel.h>

/**
  * @class PeerListDelegate
  *
  * The purpose of this class is mainly to draw the little chart which shows the download rate and upload rate of each peer.
  * The information used to draw this chart is stored in a 'PeerListModel::TransferInformation' object.
  */

void PeerListDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QStyleOptionViewItemV4 newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);

   // Show the selection only if the widget is active.
   if (!(newOption.state & QStyle::State_Active))
      newOption.state = newOption.state & (~QStyle::State_Selected);

   QStyledItemDelegate::paint(painter, newOption, index);

   if (index.column() == 0)
   {
      static quint32 LAN_SPEED = SETTINGS.get<quint32>("lan_speed");

      PeerListModel::TransferInformation transferInformation = index.data().value<PeerListModel::TransferInformation>();
      painter->setRenderHint(QPainter::Antialiasing, true);
      const QPoint center = option.rect.center();
      const int radius = qMin(option.rect.height(), option.rect.width()) / 2 - 2;
      const QRect rect(center + QPoint(-radius, -radius), center + QPoint(radius, radius));

      static const QColor DOWNLOAD_COLOR(100, 255, 100);
      static const QColor UPLOAD_COLOR(100, 100, 255);

      // Backgrounds
      painter->setPen(Qt::NoPen);
      painter->setBrush(QBrush(DOWNLOAD_COLOR.lighter(140)));
      painter->drawPie(rect, 0, 16 * 180);
      painter->setBrush(QBrush(UPLOAD_COLOR.lighter(140)));
      painter->drawPie(rect, 0, -16 * 180);

      // Download speed
      if (transferInformation.downloadRate > 0)
      {
         painter->setPen(Qt::NoPen);
         painter->setBrush(QBrush(DOWNLOAD_COLOR.darker(180)));
         const int downloadAngle = 16.0 * 180.0 * (log10(double(transferInformation.downloadRate) / double(LAN_SPEED)) + 1.5) / 1.5; // Logarithmic scale.
         //const int downloadAngle = -(16LL * 180 * transferInformation.downloadRate) / LAN_SPEED; // Linear scale.
         if (downloadAngle > 0)
            painter->drawPie(rect, 16 * 180, downloadAngle > 16 * 180 ? -16 * 180 : -downloadAngle);
      }

      // Upload speed
      if (transferInformation.uploadRate > 0)
      {
         painter->setPen(Qt::NoPen);
         painter->setBrush(QBrush(UPLOAD_COLOR.darker(180)));
         const int uploadAngle = 16.0 * 180.0 * (log10(double(transferInformation.uploadRate) / double(LAN_SPEED)) + 1.5) / 1.5; // Logarithmic scale.
         //const int uploadAngle = (16LL * 180 * transferInformation.uploadRate) / LAN_SPEED; // Linear scale.
         if (uploadAngle > 0)
            painter->drawPie(rect, 16 * 180, uploadAngle > 16 * 180 ? 16 * 180 : uploadAngle);
      }

      painter->setPen(QPen(QBrush(transferInformation.isDownloadingOurData ? QColor(220, 220, 0) : QColor(150, 150, 150)), transferInformation.isDownloadingOurData ? 1.5 : 1.2));
      painter->setBrush(Qt::NoBrush);
      painter->drawEllipse(rect);
   }
}

QSize PeerListDelegate::sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   if (index.column() == 0)
   {
      return QSize(16, 16);
   }
   else
   {
      return QStyledItemDelegate::sizeHint(option, index);
   }
}
