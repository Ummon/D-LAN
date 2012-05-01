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
  
#include <Taskbar/TaskbarImplWin.h>
using namespace GUI;

TaskbarImplWin::TaskbarImplWin() :
   winHandle(nullptr),
   taskbarInterface(nullptr)
{
   this->iDTaskbarButtonCreated = RegisterWindowMessage(L"TaskbarButtonCreated");
}

TaskbarImplWin::~TaskbarImplWin()
{
   if (this->taskbarInterface)
      this->taskbarInterface->Release();
}

void TaskbarImplWin::setStatus(TaskbarButtonStatus status)
{
   if (!this->winHandle || !this->taskbarInterface)
      return;

   switch (status)
   {
   case TaskbarButtonStatus::BUTTON_STATUS_INDETERMINATE:
      this->taskbarInterface->SetProgressState(this->winHandle, TBPF_INDETERMINATE);
      break;
   case TaskbarButtonStatus::BUTTON_STATUS_NORMAL:
      this->taskbarInterface->SetProgressState(this->winHandle, TBPF_NORMAL);
      break;
   case TaskbarButtonStatus::BUTTON_STATUS_ERROR:
      this->taskbarInterface->SetProgressState(this->winHandle, TBPF_ERROR);
      break;
   case TaskbarButtonStatus::BUTTON_STATUS_PAUSED:
      this->taskbarInterface->SetProgressState(this->winHandle, TBPF_PAUSED);
      break;
   case TaskbarButtonStatus::BUTTON_STATUS_NOPROGRESS:
      this->taskbarInterface->SetProgressState(this->winHandle, TBPF_NOPROGRESS);
      break;
   }
}

void TaskbarImplWin::setProgress(quint64 completed, quint64 total)
{
   if (!this->winHandle || !this->taskbarInterface)
      return;

   this->taskbarInterface->SetProgressValue(this->winHandle, completed, total);
}

void TaskbarImplWin::setOverlayIcon(const QIcon& icon, const QString& description)
{
   if (!this->winHandle || !this->taskbarInterface)
      return;

   HICON overlayIcon = icon.isNull() ? NULL : icon.pixmap(48).toWinHICON();
   this->taskbarInterface->SetOverlayIcon(this->winHandle, overlayIcon, description.toStdWString().c_str());

   if (overlayIcon)
      DestroyIcon(overlayIcon);
}

void TaskbarImplWin::setWinHandle(HWND winHandle)
{
   this->winHandle = winHandle;
}

void TaskbarImplWin::winEvent(MSG* message, long* /*result*/)
{
   if (message->message == iDTaskbarButtonCreated)
   {
      this->initTaskbarButton();
   }
}

void TaskbarImplWin::initTaskbarButton()
{
   HRESULT hr = CoCreateInstance(CLSID_TaskbarList, NULL, CLSCTX_INPROC_SERVER, IID_ITaskbarList3, reinterpret_cast<void**>(&(this->taskbarInterface)));

   if (SUCCEEDED(hr))
   {
      hr = this->taskbarInterface->HrInit();

      if (FAILED(hr))
      {
        this->taskbarInterface->Release();
        this->taskbarInterface = nullptr;
      }
   }
}

