package SIFT

import Chisel._

/*  it: provides width, height, and depth info on img
  * index: the index of this gaussian
  * n_ext: number of possible extrema output from this octave
  * next_tap: what point in the gaussian stream to send to the next octave
  */
class Octave(it: ImageType, index: Int, n_ext: Int = 2, next_tap: Int = 2)
  extends Module {
  
  val io = new Bundle {
    val reset = Bool(INPUT)
    val in = Valid(UInt(width=it.dwidth)).asInput
    val coord = Valid(new Coord(it)).asOutput
    
    // Debug image selection and output
    val select = Decoupled(UInt,width=8))
    val img = Valid(UInt(width=it.dwidth)).asOutput

    // Chain output and input
    val next_img_out = Decoupled(UInt(width=it.dwdith)).asOutput
    val next_img_in = Decoupled(UInt(width=it.dwdith)).asInput
  }

  // Count pixels output
  val ic = Module(new ImageCounter(it))
  io.coord.bits <> ic.io.out
  ic.io.reset := io.reset | io.select.fire
  ic.io.en := io.img.fire

  // Allow changing source when stream is not in process
  val select_ready = Reg(resetVal = Bool(true))
  io.select.ready := select_ready
  when(io.img.fire & ic.io.top)
    select_ready := Bool(true)
  when(io.in.valid)
    select_ready := Bool(false)

  // Latch output image source when allowed
  val select_r = Reg(resetVal = UInt(0,8))
  when (io.select.fire)
    select_r := io.select.bits

  // Downsampler
  val ds = Module(new DownSampler(it))
  ds.io.reset := io.reset
  ds.io.in <> io.in

  val it_div_2 = new ImageType(it.width>>UInt(1), it.height >> UInt(1),
    itd.depth)

  // Chain of gaussian blurs
  val n_gauss = n_ext + 3
  val gauss = Range(0, n_gauss).map(i => Module(new Gaussian(it_div_2)))
  
  gauss(0).io.in <> ds.io.out

  for(i <- 0 until n_gauss) {
    gauss(i).io.reset := io.reset
    if (i < n_gauss - 1)
      gauss(i+1).io.in <> gauss(i).io.out
  }

  io.next_img_out <> gauss(next_tap).io.out

  // Take difference of gaussian pairs
  val n_diff = n_ext + 2
  val diff = Range(0, n_diff).map(i => Module(new DelayDiff(it_div_2)))
  for (i <- 0 until n_diff) {
    diff(i).io.reset := io.reset
    diff(i).io.a <> gauss(i).io.out
    diff(i).io.b <> gauss(i+1).io.out
  }

  // Upsampler
  val us = Module(new UpSampler(it_div_2))

  // Debug image output stream selection
  // When our index is the active source, select an internal stream
  when(select_r(7,4) === UInt(index)) {
    // 0 is bypasses down/upsample, helps debug tap select
    when(select_r(3,0) === UInt(0)) {
      io.img <> io.in
    // Otherwise use output from upsampler
    } .otherwise {
      io.img <> us.io.out
    }

    // Default input to upsampler is downsampler, includes select_r = 1
    us.io.in <> ds.io.out
    switch(select_r(3,0)) {
      is(UInt(2))   {us.io.in <> gauss(0).io.out
      is(UInt(3))   {us.io.in <> gauss(1).io.out
      is(UInt(4))   {us.io.in <> diff(0).io.out
      is(UInt(5))   {us.io.in <> gauss(2).io.out
      is(UInt(6))   {us.io.in <> diff(1).io.out
      is(UInt(7))   {us.io.in <> gauss(3).io.out
      is(UInt(8))   {us.io.in <> diff(2).io.out
      is(UInt(9))   {us.io.in <> gauss(4).io.out
      is(UInt(10))  {us.io.in <> diff(3).io.out
    }

  // Otherwise select stream from next octave and upsample it
  } .otherwise {
    us.io.in <> io.next_img_in
    io.img <> us.io.out
  }
}
