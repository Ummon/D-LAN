#include "Hash.h"
using namespace Common;

Hash::Hash()
{}

Hash::Hash(const char* str)
   : QByteArray(str)
{}
