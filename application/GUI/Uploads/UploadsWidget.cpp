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
  
#include <Uploads/UploadsWidget.h>
#include <ui_UploadsWidget.h>
using namespace GUI;

void UploadsDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   if (!index.isValid())
      return;

   if (index.column() == UploadsModel::PROGRESS)
   {
      QStyledItemDelegate::paint(painter, option, QModelIndex());

      const int progress = index.data().toInt();

      QStyleOptionProgressBar progressBarOption;
      progressBarOption.QStyleOption::operator=(option);
      progressBarOption.state |= QStyle::State_Horizontal;
      progressBarOption.minimum = 0;
      progressBarOption.maximum = 10000;
      progressBarOption.textAlignment = Qt::AlignHCenter | Qt::AlignVCenter;
      progressBarOption.progress = progress;
      progressBarOption.textVisible = true;

      const double percentProgress = static_cast<double>(progress) / 100;
      progressBarOption.text = QStringLiteral("%1%").arg(percentProgress > 100 ? 100 : percentProgress);

      QApplication::style()->drawControl(QStyle::CE_ProgressBar, &progressBarOption, painter, option.widget);
   }
   else
   {
      // Remove the focus box, not very useful.
      QStyleOptionViewItem newOption(option);
      newOption.state = option.state & (~QStyle::State_HasFocus);
      QStyledItemDelegate::paint(painter, newOption, index);
   }
}

QSize UploadsDelegate::sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QSize size = QStyledItemDelegate::sizeHint(option, index);

   if (index.column() == UploadsModel::PROGRESS)
      size.setWidth(100);
   return size;
}

/////

UploadsWidget::UploadsWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, QWidget* parent) :
   QWidget(parent),
   ui(new Ui::UploadsWidget),
   uploadsModel(coreConnection, peerListModel)
{
   this->ui->setupUi(this);

   this->ui->tblUploads->setModel(&this->uploadsModel);
   this->ui->tblUploads->setItemDelegate(&this->uploadsDelegate);
   this->ui->tblUploads->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);

   this->ui->tblUploads->horizontalHeader()->setStretchLastSection(false);
   this->ui->tblUploads->horizontalHeader()->setVisible(false);
   this->ui->tblUploads->horizontalHeader()->setSectionResizeMode(UploadsModel::FILENAME, QHeaderView::Stretch);
   this->ui->tblUploads->horizontalHeader()->setSectionResizeMode(UploadsModel::PROGRESS, QHeaderView::ResizeToContents);
   this->ui->tblUploads->horizontalHeader()->setSectionResizeMode(UploadsModel::PEER, QHeaderView::ResizeToContents);
   this->ui->tblUploads->horizontalHeader()->setMinimumSectionSize(0);

   //this->ui->tblChat->verticalHeader()->setResizeMode(QHeaderView::ResizeToContents);
   this->ui->tblUploads->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);
   this->ui->tblUploads->verticalHeader()->setDefaultSectionSize(QFontMetrics(QApplication::font()).height() + 2);
   this->ui->tblUploads->verticalHeader()->setVisible(false);

   this->ui->tblUploads->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblUploads->setSelectionMode(QAbstractItemView::SingleSelection);
   this->ui->tblUploads->setShowGrid(false);
   this->ui->tblUploads->setAlternatingRowColors(true);
}

UploadsWidget::~UploadsWidget()
{
   delete this->ui;
}

void UploadsWidget::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
      this->ui->retranslateUi(this);

   QWidget::changeEvent(event);
}

