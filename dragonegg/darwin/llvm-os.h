/* Darwin specific definitions
Copyright (C) 2009 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef LLVM_OS_H
#define LLVM_OS_H

/* Darwin X86-64 only supports PIC code generation. */
#if defined (TARGET_386)
#define LLVM_SET_TARGET_OPTIONS(argvec)              \
  if ((TARGET_64BIT) || flag_pic)                    \
    argvec.push_back ("--relocation-model=pic");     \
  else if (!MACHO_DYNAMIC_NO_PIC_P)                  \
    argvec.push_back ("--relocation-model=static")
#elif defined (TARGET_ARM)
#define LLVM_SET_TARGET_OPTIONS(argvec)              \
  if (flag_pic)                                      \
    argvec.push_back ("--relocation-model=pic");     \
  else if (!MACHO_DYNAMIC_NO_PIC_P)                  \
    argvec.push_back ("--relocation-model=static");  \
#else /* !TARGET_386 && !TARGET_ARM */
#define LLVM_SET_TARGET_OPTIONS(argvec)              \
  if (flag_pic)                                      \
    argvec.push_back ("--relocation-model=pic");     \
  else if (!MACHO_DYNAMIC_NO_PIC_P)                  \
    argvec.push_back ("--relocation-model=static")
#endif /* !TARGET_386 && !TARGET_ARM */

/* Give a constant string a sufficient alignment for the platform.  */
#define TARGET_ADJUST_CSTRING_ALIGN(GV)                                 \
  do {                                                                  \
    if (GV->hasInternalLinkage()) {                                     \
      GV->setAlignment(TARGET_64BIT ? 8 : 4);                           \
    }                                                                   \
  } while (0)

#endif /* LLVM_OS_H */
