/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2017 TrustInSoft                                      */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

#include <stdint.h>

#define __TIS_ATOMIC_INIT(typ) \
    void __c11_atomic_init_##typ(volatile _Atomic(typ) *a, typ b) {     \
        *a = b;                                         \
        return;                                         \
    };

__TIS_ATOMIC_INIT(int8_t)
__TIS_ATOMIC_INIT(int16_t)
__TIS_ATOMIC_INIT(int32_t)
__TIS_ATOMIC_INIT(int64_t)
__TIS_ATOMIC_INIT(uint8_t)
__TIS_ATOMIC_INIT(uint16_t)
__TIS_ATOMIC_INIT(uint32_t)
__TIS_ATOMIC_INIT(uint64_t)
