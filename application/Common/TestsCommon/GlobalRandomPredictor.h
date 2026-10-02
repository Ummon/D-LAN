#pragma once

#include <random>
#include <sstream>

#include <QRandomGenerator>

/**
  * 'QRandomGenerator::global()' is a Mersenne Twister (std::mt19937): its state can be rebuilt from 624 of its
  * outputs, which then predicts all its next ones. Used to check that secrets aren't taken from it.
  */
class GlobalRandomPredictor
{
public:
   // Consumes 624 outputs of 'QRandomGenerator::global()'.
   GlobalRandomPredictor()
   {
      std::stringstream state;
      for (int i = 0; i < 624; ++i)
         state << untemper(QRandomGenerator::global()->generate()) << ' ';
      state >> this->engine;
   }

   // Consumes one output of 'QRandomGenerator::global()'. False if Qt has replaced its algorithm.
   bool followsGlobal()
   {
      return QRandomGenerator::global()->generate() == this->engine();
   }

   // Whether 'value' is made of two consecutive outputs among the next ones of 'QRandomGenerator::global()'.
   bool predicts(quint64 value, int window = 4096) const
   {
      std::mt19937 next = this->engine;
      quint32 previous = next();
      for (int i = 1; i < window; ++i)
      {
         const quint32 current = next();
         if (value == (quint64(current) << 32 | previous) || value == (quint64(previous) << 32 | current))
            return true;
         previous = current;
      }
      return false;
   }

private:
   // Inverse of the std::mt19937 output tempering.
   static quint32 untemper(quint32 y)
   {
      y ^= y >> 18;
      y ^= (y << 15) & 0xefc60000u;
      quint32 x = y;
      for (int i = 0; i < 4; ++i)
         x = y ^ ((x << 7) & 0x9d2c5680u);
      y = x;
      for (int i = 0; i < 2; ++i)
         x = y ^ (x >> 11);
      return x;
   }

   std::mt19937 engine;
};
