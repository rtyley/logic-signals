package com.madgag.logic

import scodec.bits.BitVector

enum BitEndian(val convert: Iterable[Boolean] => BitVector):
  case LittleFirst extends BitEndian(BitVector.bits(_).reverse)
  case BigFirst extends BitEndian(BitVector.bits)
