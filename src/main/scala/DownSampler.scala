package SIFT

import Chisel._

class DownSampler(params: SSEParams) extends Module {
  val io = new Bundle {
    val in = Decoupled(UInt(width=params.it.dwidth)).flip
    val out = Decoupled(UInt(width=params.it.dwidth))
  }

  io.in.ready := io.out.ready

  io.out.bits := io.in.bits

  val col_counter = Module(new Counter(params.it.width-1))
  col_counter.io.en := io.in.fire()

  val row_active = Reg(init = Bool(true))
  when(io.in.fire() & col_counter.io.top) {
    row_active := ~row_active
  }

  io.out.valid := ~col_counter.io.count(0) & row_active & io.in.valid
}

class DownSamplerTester(c: DownSampler) extends Tester(c, false) {

  val width = c.params.it.width
  val height = c.params.it.height
  val dwidth = c.params.it.dwidth

  val n_byte = dwidth/8
  val n_pixel = width * height
  val n_pixel_ds = width/2 * height/2

  val random_img = Image(width, height, dwidth)
  val rng = new scala.util.Random()
  rng.nextBytes(random_img.data)

  val img_out = Image(width/2, height/2, dwidth)
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

    while ((time < timeout) && (in_idx < n_pixel || out_idx < n_pixeli_ds)) {
      
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
      
      if((out_idx < n_pixel_ds) && peek(c.io.img_out.valid)==1) {
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

  def downsampler(img_in: Image) = {  
    val img_ds = Image(img_in.w/2, img_in.h/2, img_in.d)
    for (i <- 0 until img_ds.h) {
      for (j <- 0 until img_ds.w) {
        img_ds.data(i*img_ds.w + j) = img_in.data(2*i*img_in.w + 2*j)
      }
    }
    img_ds
  }

  process(random_img, timeout)
  val img_exp = downsampler(random_img)
  val passed = (time < timeout) && check_img_out(img_exp)
  println("Check %b".format(passed))
  if (passed){
    any_passed = true
  } else {
    all_passed = false
  }
  ok = all_passed && any_passed
}
