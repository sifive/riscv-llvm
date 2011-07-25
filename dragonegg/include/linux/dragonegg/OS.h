//===------------- OS.h - Linux specific definitions ------------*- C++ -*-===//
//
// Copyright (C) 2009, 2010, 2011  Duncan Sands et al.
//
// This file is part of DragonEgg.
//
// DragonEgg is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later version.
//
// DragonEgg is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// DragonEgg; see the file COPYING.  If not, write to the Free Software
// Foundation, 51 Franklin Street, Suite 500, Boston, MA 02110-1335, USA.
//
//===----------------------------------------------------------------------===//
// This file provides Linux specific declarations.
//===----------------------------------------------------------------------===//

#ifndef DRAGONEGG_OS_H
#define DRAGONEGG_OS_H

/* Yes, we support PIC codegen for linux targets! */
#define LLVM_SET_RELOC_MODEL(RelocModel)	\
  if (flag_pic)					\
    RelocModel = Reloc::PIC_;			\
  else						\
    RelocModel = Reloc::Static;

#endif /* DRAGONEGG_OS_H */
