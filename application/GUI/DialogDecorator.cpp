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

#include <DialogDecorator.h>
using namespace GUI;

#include <QApplication>
#include <QHBoxLayout>
#include <QLabel>
#include <QLayout>
#include <QMessageBox>
#include <QMouseEvent>
#include <QPushButton>
#include <QWindow>

#include <WindowFrame.h>

namespace
{
   /**
     * The title bar and the frame of a dialog, see 'GUI::DialogDecorator'.
     */
   class DialogDecoration : public QObject
   {
   public:
      DialogDecoration(QDialog* dialog, int frameWidth);

   protected:
      bool eventFilter(QObject* obj, QEvent* event) override;

   private:
      void updateGeometry();

      QDialog* const dialog;
      const int frameWidth;
      QWidget* const titleBar;
      QLabel* const title;
      QPoint dragPosition;
   };

   DialogDecoration::DialogDecoration(QDialog* dialog, int frameWidth) :
      QObject(dialog), dialog(dialog), frameWidth(frameWidth), titleBar(new QWidget(dialog)), title(new QLabel(dialog->windowTitle(), this->titleBar))
   {
      this->titleBar->setObjectName("dialogTitleBar");
      this->title->setObjectName("dialogTitle");

      // As the system draws the titles. A style sheet doesn't change this property of the font.
      QFont font = this->title->font();
      font.setStyleStrategy(QFont::NoAntialias);
      this->title->setFont(font);

      auto* closeButton = new QPushButton(this->titleBar);
      closeButton->setObjectName("butDialogClose");
      closeButton->setFocusPolicy(Qt::NoFocus);
      closeButton->setAutoDefault(false);
      connect(closeButton, &QPushButton::clicked, dialog, &QWidget::close);

      auto* layout = new QHBoxLayout(this->titleBar);
      layout->setContentsMargins(0, 0, 0, 0);
      layout->setSpacing(0);
      layout->addWidget(this->title, 1);
      layout->addWidget(closeButton);

      // The style sheet gives the size of the title bar.
      this->titleBar->ensurePolished();

      dialog->installEventFilter(this);
      this->titleBar->installEventFilter(this);
      dialog->setMouseTracking(true);
      new WindowFrame(dialog);

      this->updateGeometry();
   }

   bool DialogDecoration::eventFilter(QObject* obj, QEvent* event)
   {
      if (obj == this->dialog)
      {
         if (event->type() == QEvent::Resize)
            this->updateGeometry();
         else if (event->type() == QEvent::WindowTitleChange)
            this->title->setText(this->dialog->windowTitle());
      }
      // The title bar moves the dialog, as the one of the main window does (see 'MainWindow::eventFilter(..)').
      else if (obj == this->titleBar)
      {
         const QMouseEvent* mouseEvent = static_cast<QMouseEvent*>(event);
         switch (event->type())
         {
         case QEvent::MouseButtonPress:
            if (mouseEvent->button() == Qt::LeftButton)
               this->dragPosition = mouseEvent->globalPosition().toPoint() - this->dialog->frameGeometry().topLeft();
            break;

         case QEvent::MouseButtonRelease:
            this->dragPosition = QPoint();
            break;

         case QEvent::MouseMove:
            if (mouseEvent->buttons() & Qt::LeftButton && !this->dragPosition.isNull())
            {
               if (this->dialog->windowHandle() && this->dialog->windowHandle()->startSystemMove())
                  this->dragPosition = QPoint();
               else
                  this->dialog->move(mouseEvent->globalPosition().toPoint() - this->dragPosition);
            }
            break;

         default:;
         }
      }

      return false;
   }

   /**
     * The title bar is over the top margin of the dialog, the layout of the dialog takes the rest.
     */
   void DialogDecoration::updateGeometry()
   {
      const int titleBarHeight = this->titleBar->sizeHint().height();
      this->titleBar->setGeometry(this->frameWidth, this->frameWidth, this->dialog->width() - 2 * this->frameWidth, titleBarHeight);
      this->dialog->setContentsMargins(this->frameWidth, this->frameWidth + titleBarHeight, this->frameWidth, this->frameWidth);
   }
}

/**
  * @class GUI::DialogDecorator
  *
  * Replaces the decoration drawn by the system around the dialogs with a title bar and a frame drawn by the style sheet:
  *  - The dialogs have the property 'decoration' set to "fixed" or "resizable" (used by the style sheet to draw the frame).
  *  - The title bar is named "dialogTitleBar", its title "dialogTitle" and its close button "butDialogClose".
  * Only the dialogs shown after a call to 'setFrameWidths(..)' with a width are decorated, the others keep their decoration.
  */

DialogDecorator::DialogDecorator(QObject* parent) :
   QObject(parent), fixedSizeFrameWidth(0), resizableFrameWidth(0)
{
}

/**
  * @param fixedSizeFrameWidth The width of the frame of the dialogs with a fixed size, 0 to stop decorating the dialogs.
  * @param resizableFrameWidth The width of the frame of the other dialogs, the frame can be dragged to resize them.
  */
void DialogDecorator::setFrameWidths(int fixedSizeFrameWidth, int resizableFrameWidth)
{
   this->fixedSizeFrameWidth = fixedSizeFrameWidth;
   this->resizableFrameWidth = resizableFrameWidth;

   if (fixedSizeFrameWidth > 0)
      qApp->installEventFilter(this);
   else
      qApp->removeEventFilter(this);
}

bool DialogDecorator::eventFilter(QObject* obj, QEvent* event)
{
   if (event->type() == QEvent::Polish)
   {
      QDialog* dialog = qobject_cast<QDialog*>(obj);
      if (dialog && dialog->isWindow() && !dialog->property("decoration").isValid())
         this->decorate(dialog);
   }
   return false;
}

void DialogDecorator::decorate(QDialog* dialog)
{
   // A message box gets its fixed size when it's shown.
   const bool fixedSize =
      qobject_cast<QMessageBox*>(dialog) || dialog->minimumSize() == dialog->maximumSize() ||
      (dialog->layout() && dialog->layout()->sizeConstraint() == QLayout::SetFixedSize);

   // Set before the style polishes the dialog.
   dialog->setProperty("decoration", fixedSize ? "fixed" : "resizable");

   const Qt::WindowFlags flags = dialog->windowFlags() | Qt::FramelessWindowHint;
   if (dialog->windowHandle())
   {
      // The native window is created before the dialog is polished, it's changed without being created again.
      dialog->overrideWindowFlags(flags);
      dialog->windowHandle()->setFlags(flags);
   }
   else
      dialog->setWindowFlags(flags);

   new DialogDecoration(dialog, fixedSize ? this->fixedSizeFrameWidth : this->resizableFrameWidth);
}
