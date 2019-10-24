// RUN: not llvm-mc -triple=riscv64 -mattr=+v < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR

vsetvli a2, a0, e31
// CHECK-ERROR: operand must be e[8|16|32|64|128|256|512|1024],m[1|2|4|8]

vsetvli a2, a0, e32,m3
// CHECK-ERROR: operand must be e[8|16|32|64|128|256|512|1024],m[1|2|4|8]

vsetvli a2, a0, m1,e32
// CHECK-ERROR: operand must be e[8|16|32|64|128|256|512|1024],m[1|2|4|8]

vsetvli a2, a0, e32,m16
// CHECK-ERROR: operand must be e[8|16|32|64|128|256|512|1024],m[1|2|4|8]

vsetvli a2, a0, e2048,m8
// CHECK-ERROR: operand must be e[8|16|32|64|128|256|512|1024],m[1|2|4|8]

vsetvli a2, a0, e1,m8
// CHECK-ERROR: operand must be e[8|16|32|64|128|256|512|1024],m[1|2|4|8]

vadd.vv v1, v3, v2, v4.t
// CHECK-ERROR: operand must be v0.t

vadd.vv v1, v3, v2, v0
// CHECK-ERROR: expected '.t' suffix

vwaddu.vv v0, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwaddu.vx v0, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwsubu.vv v0, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwsubu.vx v0, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwadd.vv v0, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwadd.vx v0, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwsub.vv v0, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwsub.vx v0, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwmul.vv v0, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwmul.vx v0, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwmulu.vv v0, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwmulu.vx v0, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwmulsu.vv v0, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwmulsu.vx v0, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwmaccu.vv v0, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwmaccu.vx v0, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwmacc.vv v0, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwmacc.vx v0, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwmaccsu.vv v0, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwmaccsu.vx v0, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwmaccus.vx v0, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwsmaccu.vv v0, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwsmaccu.vx v0, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwsmacc.vv v0, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwsmacc.vx v0, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwsmaccsu.vv v0, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwsmaccsu.vx v0, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwsmaccus.vx v0, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwadd.vv v0, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwadd.vf v0, v1, fa0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwsub.vv v0, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwsub.vf v0, v1, fa0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwmul.vv v0, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwmul.vf v0, v1, fa0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwmacc.vv v0, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwmacc.vf v0, fa0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwnmacc.vv v0, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwnmacc.vf v0, fa0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwmsac.vv v0, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwmsac.vf v0, fa0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwnmsac.vv v0, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwnmsac.vf v0, fa0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vrgather.vv v0, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vrgather.vx v0, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vrgather.vi v0, v1, 31, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vwaddu.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwaddu.vx v1, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsubu.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsubu.vx v1, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwadd.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwadd.vx v1, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsub.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsub.vx v1, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmul.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmul.vx v1, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmulu.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmulu.vx v1, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmulsu.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmulsu.vx v1, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmaccu.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmaccu.vx v1, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmacc.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmacc.vx v1, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmaccsu.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmaccsu.vx v1, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmaccus.vx v1, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmaccu.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmaccu.vx v1, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmacc.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmacc.vx v1, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmaccsu.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmaccsu.vx v1, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmaccus.vx v1, a0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwadd.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwadd.vf v1, v1, fa0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwsub.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwsub.vf v1, v1, fa0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwmul.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwmul.vf v1, v1, fa0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwmacc.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwmacc.vf v1, fa0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwnmacc.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwnmacc.vf v1, fa0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwmsac.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwmsac.vf v1, fa0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwnmsac.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwnmsac.vf v1, fa0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vrgather.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vrgather.vx v1, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vrgather.vi v1, v1, 31, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwaddu.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsubu.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwadd.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsub.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmul.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmulu.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmulsu.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmaccu.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmacc.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmaccsu.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmaccu.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmacc.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmaccsu.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwadd.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwsub.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwmul.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwmacc.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwnmacc.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwmsac.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwnmsac.vv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vrgather.vv v1, v2, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vrgather.vi v1, v1, 31, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwaddu.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsubu.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwadd.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsub.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmul.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmulu.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmulsu.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmaccu.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmacc.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmaccsu.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmaccu.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmacc.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmaccsu.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwadd.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwsub.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwmul.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwmacc.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwnmacc.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwmsac.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwnmsac.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vrgather.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vrgather.vi v1, v1, 31
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwaddu.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsubu.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwadd.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsub.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmul.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmulu.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmulsu.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmaccu.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmacc.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwmaccsu.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmaccu.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmacc.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vwsmaccsu.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwadd.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwsub.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwmul.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwmacc.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwnmacc.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwmsac.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwnmsac.vv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vrgather.vv v1, v2, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vcompress.vm v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vcompress.vm v2, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vrgather.vi v1, v1, 31, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vrgather.vi v0, v1, 31, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwcvt.xu.f.v v1, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwcvt.x.f.v v1, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwcvt.f.xu.v v1, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwcvt.f.x.v v1, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwcvt.f.f.v v1, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

viota.m v1, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnsrl.wv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnsrl.wx v1, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnsrl.wi v1, v1, 31, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnsra.wv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnsra.wx v1, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnsra.wi v1, v1, 31, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnclipu.wv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnclipu.wx v1, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnclipu.wi v1, v1, 31, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnclip.wv v1, v1, v2, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnclip.wx v1, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnclip.wi v1, v1, 31, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfncvt.xu.f.w v1, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfncvt.x.f.w v1, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfncvt.f.xu.w v1, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfncvt.f.x.w v1, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfncvt.f.f.w v1, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfncvt.rod.f.f.w v1, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwcvt.xu.f.v v0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwcvt.x.f.v v0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwcvt.f.xu.v v0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwcvt.f.x.v v0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwcvt.f.f.v v0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

viota.m v0, v1, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the mask register.

vfwcvt.xu.f.v v1, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwcvt.x.f.v v1, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwcvt.f.xu.v v1, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwcvt.f.x.v v1, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfwcvt.f.f.v v1, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

viota.m v1, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnsrl.wv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnsrl.wx v1, v1, a0
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnsrl.wi v1, v1, 31
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnsra.wv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnsra.wx v1, v1, a0
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnsra.wi v1, v1, 31
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnclipu.wv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnclipu.wx v1, v1, a0
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnclipu.wi v1, v1, 31
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnclip.wv v1, v1, v2
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnclip.wx v1, v1, a0
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vnclip.wi v1, v1, 31
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfncvt.xu.f.w v1, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfncvt.x.f.w v1, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfncvt.f.xu.w v1, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfncvt.f.x.w v1, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfncvt.f.f.w v1, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vfncvt.rod.f.f.w v1, v1
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vslideup.vx v1, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vslideup.vi v1, v1, 31, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vslide1up.vx v1, v1, a0, v0.t
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vslideup.vx v1, v1, a0
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vslideup.vi v1, v1, 31
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.

vslide1up.vx v1, v1, a0
// CHECK-ERROR: The destination vector register group cannot overlap the source vector register group.
