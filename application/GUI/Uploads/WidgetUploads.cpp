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
  
#include <Uploads/WidgetUploads.h>
#include <ui_WidgetUploads.h>
using namespace GUI;

void UploadsDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   if (!index.isValid())
      return;

   if (index.column() == 2)
   {
      QStyleOptionProgressBarV2 progressBarOption;
      progressBarOption.QStyleOption::operator=(option);

      progressBarOption.minimum = 0;
      progressBarOption.maximum = 10000;
      progressBarOption.textAlignment = Qt::AlignHCenter;
      progressBarOption.progress = index.data().toInt();
      progressBarOption.textVisible = false;

      QApplication::style()->drawControl(QStyle::CE_ProgressBar, &progressBarOption, painter, &this->model);
   }
   else
   {
      // Remove the focus box, not very useful.
      QStyleOptionViewItemV4 newOption(option);
      newOption.state = option.state & (~QStyle::State_HasFocus);
      QStyledItemDelegate::paint(painter, newOption, index);
   }
}

QSize UploadsDelegate::sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QSize size = QStyledItemDelegate::sizeHint(option, index);

   if (index.column() == 2)
      size.setWidth(100);
   return size;
}

/////

WidgetUploads::WidgetUploads(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, QWidget* parent) :
   QWidget(parent), ui(new Ui::WidgetUploads), uploadsModel(coreConnection, peerListModel)
{
   this->ui->setupUi(this);

   this->ui->tblUploads->setModel(&this->uploadsModel);
   this->ui->tblUploads->setItemDelegate(&this->uploadsDelegate);
   this->ui->tblUploads->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);
   this->ui->tblUploads->horizontalHeader()->setVisible(false);
   this->ui->tblUploads->horizontalHeader()->setResizeMode(0, QHeaderView::Stretch);
   this->ui->tblUploads->horizontalHeader()->setResizeMode(1, QHeaderView::ResizeToContents);
   this->ui->tblUploads->horizontalHeader()->setResizeMode(2, QHeaderView::ResizeToContents);
   this->ui->tblUploads->horizontalHeader()->setResizeMode(3, QHeaderView::ResizeToContents);

   //this->ui->tblChat->verticalHeader()->setResizeMode(QHeaderView::ResizeToContents);
   this->ui->tblUploads->verticalHeader()->setResizeMode(QHeaderView::Fixed);
   this->ui->tblUploads->verticalHeader()->setDefaultSectionSize(QApplication::fontMetrics().height() + 2);

   this->ui->tblUploads->verticalHeader()->setVisible(false);
   this->ui->tblUploads->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblUploads->setSelectionMode(QAbstractItemView::SingleSelection);
   this->ui->tblUploads->setShowGrid(false);
   this->ui->tblUploads->setAlternatingRowColors(true);
}

WidgetUploads::~WidgetUploads()
{
   delete this->ui;
}

void WidgetUploads::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
      this->ui->retranslateUi(this);
   else
      QWidget::changeEvent(event);
}

