package SIFT

import Chisel._

class UpSampler(params: SSEParams) extends Module{
  val io = new Bundle{
    val in = Decoupled(UInt(width=params.it.dwidth)).flip
    val out = Decoupled(UInt(width=params.it.dwidth))
  }

  val buf = Mem(UInt(width=params.it.dwidth), params.it.width)

  val out_col = Module(new Counter(params.it.width*2-1))
  val out_idx = out_col.io.count >> UInt(1)

  val in_idx = Module(new Counter(params.it.width-1))
  
  val maybe_full = Reg(init = Bool(false))
  val ptr_match = in_idx.io.count === out_idx
  
  when (io.in.fire() != (out_col.io.count(0) & io.out.fire())) {
    maybe_full := io.in.fire()
  }

  val empty = Bool()
  val full = Bool()

  val s_normal :: s_wait :: s_pass :: Nil = Enum(UInt(),3)
  val state = Reg(init = s_normal)
  
  val state_out = UInt()
  state_out := state

  empty := Bool(false)
  full := Bool(true)

  switch(state_out) {
    is(s_normal) {
      empty := ptr_match & !maybe_full & !out_col.io.count(0)
      full := ptr_match & maybe_full
      
      when (in_idx.io.top & io.in.fire()) {
        state := s_wait
      }
    }
    
    // Row has been read, wait for output to complete row
    is(s_wait) {
      empty := Bool(false)
      full := Bool(true)

      when (out_col.io.top & io.out.fire()) {
        state := s_pass
      }
    }
    
    // Ensure output pointer passes input pointer to duplicate row
    is(s_pass) {
      empty := Bool(false)
      full := Bool(true)

      when(out_col.io.count(0) & io.out.fire()) {
        state := s_normal
      }
    }
  }

  out_col.io.en := io.out.fire()

  in_idx.io.en := io.in.fire()

  io.in.ready := !full
  io.out.valid := !empty

  io.out.bits := buf(out_idx)
  when(io.in.fire()) {
    buf(in_idx.io.count) := io.in.bits
  }
}

class UpSamplerTester(c: UpSampler) extends Tester(c, false) {

  val width = c.params.it.width
  val height = c.params.it.height
  val dwidth = c.params.it.dwidth

  val n_byte = dwidth/8
  val n_pixel = width * height
  val n_pixel_us = width*2 * height*2

  val random_img = Image(width, height, dwidth)
  val rng = new scala.util.Random()
  rng.nextBytes(random_img.data)

  val img_out = Image(width*2, height*2, dwidth)
  val time = 0
  val timeout = 1000

  var any_passed = false
  var all_passed = true

  def process(img_in: Image, timeout: Int = 1000) = {
      
    var in_idx = 0
    var out_idx = 0
    
    var triplet = 0
    var pixel = 0
    
    reset()
    step(1)
    
    poke(c.io.in.valid, 0)
 
    step(1)

    while ((time < timeout) && (in_idx < n_pixel || out_idx < n_pixeli_us)) {
      
      if (in_idx < n_pixel) {
        triplet = 0
        for (j <- 0 until n_byte) {
          pixel = img_in.data(n_byte*in_idx+j)
          if (pixel < 0) pixel += 256
          triplet += pixel << (8*j)
        }
        
        poke(c.io.img_in.bits, triplet)
        poke(c.io.img_in.valid, 1)
      } else {
        poke(c.io.img_in.valid, 0)
      }

      //poke(c.io.select.valid, 0)
      poke(c.io.img_out.ready, 1)
      step(1)
      
      if ((in_idx < n_pixel) && (peek(c.io.img_in.ready)==1)) {
        in_idx += 1
        time = 0
      } else {
        time += 1
      }
      
      if((out_idx < n_pixel_us) && peek(c.io.img_out.valid)==1) {
        // Write debug image out
        val out_triplet = peek(c.io.img_out.bits)

        for (j <- 0 until n_byte) {
          img_out.data((n_byte*out_idx)+j) = 
            ((out_triplet >> (8*j)) & 0xFF).toByte
        }
        
        // Color pixel red if outputting valid coord, grey otherwise
        //val coordpix = if (peek(c.io.coord.valid)==1) 0xFF0000 else 0x808080
        //for (j <- 0 until 3) {
          //img_coord.data(3*out_idx+j) = ((0xFF0000 >> (8*j)) & 0xFF).toByte
       //}

        out_idx += 1
        time = 0
      } else {
        time += 1
      }
    }
    
    poke(c.io.img_in.valid,0)
    step(10)
  }

  }  

  def check_img_out(img_exp: Image) = {
    (img_out.data, img_exp.data).zipped.map(_ == _).reduce(_ && _)
  }

  def upsampler(img_in: Image) = {  
    val img_us = Image(img_in.w*2, img_in.h*2, img_in.d)
    for (i <- 0 until img_us.h) {
      for (j <- 0 until img_us.w) {
        img_us.data(i*img_us.w + j) = img_in.data((i/2)*img_in.w + (j/2))
      }
    }
    img_us
  }

  process(random_img, timeout)
  val img_exp = upsampler(random_img)
  val passed = (time < timeout) && check_img_out(img_exp)
  println("Check %b".format(passed))
  if (passed){
    any_passed = true
  } else {
    all_passed = false
  }
  ok = all_passed && any_passed
}
