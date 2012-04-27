/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
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


const QColor PeerListDelegate::DOWNLOAD_COLOR(100, 255, 100);
const QColor PeerListDelegate::UPLOAD_COLOR(100, 100, 255);

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

      painter->drawPixmap(rect.topLeft(), this->getMiniChartBackground(rect.width(), rect.height()));

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

const QPixmap& PeerListDelegate::getMiniChartBackground(int width, int height) const
{
   if (!this->miniChartBackground)
   {
      this->miniChartBackground = new QPixmap(width, height);
      this->miniChartBackground->fill(Qt::transparent);

      QLinearGradient downloadGradient(0, 0, width, 0);
      downloadGradient.setColorAt(0, DOWNLOAD_COLOR.lighter(70));
      downloadGradient.setColorAt(1, DOWNLOAD_COLOR.lighter(160));

      QLinearGradient uploadGradient(0, 0, width, 0);
      uploadGradient.setColorAt(0, UPLOAD_COLOR.lighter(100));
      uploadGradient.setColorAt(1, UPLOAD_COLOR.lighter(160));

      QPainter painter(this->miniChartBackground);
      painter.setRenderHint(QPainter::Antialiasing, true);
      painter.setPen(Qt::NoPen);
      painter.setBrush(downloadGradient);
      painter.drawPie(QRect(0, 0, width, height), 0, 16 * 180);
      painter.setBrush(uploadGradient);
      painter.drawPie(QRect(0, 0, width, height), 0, -16 * 180);
   }
   return *this->miniChartBackground;
}
