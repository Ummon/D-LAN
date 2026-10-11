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

#include <Common/HanVariants.h>
using namespace Common;

#include <algorithm>
#include <iterator>

#include <Common/HanVariantsData.h>

/**
  * @class Common::HanVariants
  *
  * The Chinese characters have a traditional form (Taiwan, Hong Kong) and a simplified one (mainland China, Singapore),
  * e.g. 電影 and 电影. Both are the same word and have to match when searching.
  */

/**
  * Return the simplified form of a traditional Chinese character, 'c' itself if it has none.
  * Several traditional characters may share the same simplified form, e.g. 發 and 髮 => 发: that's why the simplified
  * form is the common one.
  * The simplified form of a character is the same for all its meanings, it may be wrong for some of them but the text
  * to search and the searched terms are folded the same way.
  */
char32_t HanVariants::toSimplified(char32_t c)
{
   const auto end = std::end(TRADITIONAL_TO_SIMPLIFIED);
   if (c < TRADITIONAL_TO_SIMPLIFIED[0].traditional)
      return c;

   const auto variant = std::lower_bound(
      std::begin(TRADITIONAL_TO_SIMPLIFIED),
      end,
      c,
      [](const HanVariant& variant, char32_t c) { return variant.traditional < c; }
   );
   return variant != end && variant->traditional == c ? variant->simplified : c;
}
