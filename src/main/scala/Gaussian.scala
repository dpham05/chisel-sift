package SIFT

import Chisel._

class Gaussian(params: SSEParams) extends Module{
  val io = new Bundle{
    val in = Decoupled(UInt(width=params.it.dwidth)).flip
    val out = Decoupled(UInt(width=params.it.dwidth))
  }
  
  val row_fir = Module(new SymmetricFIR(params, 1, params.it.width))
  row_fir.io.in <> io.in

  val col_fir = Module(new SymmetricFIR(params, params.it.width, params.it.height))
  col_fir.io.in <> row_fir.io.out
  io.out <> col_fir.io.out
}

class GaussianTester(c: Gaussian) extends Tester(c, false){
  val width = c.params.it.width
  val height = c.params.it.height
  val dwidth = c.params.it.dwidthi

  val n_byte = dwidth/8
  val n_pixel = width * height

  val random_img = Image(width, height, dwidth)
  val rng = new scala.util.Random()
  rng.nextBytes(random_img.data)

  val img_out = Image(width, height, dwidth)
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
    //poke(c.io.select.bits, select)
    //poke(c.io.select.valid, 1)
    step(1)
/*
    while((time < timeout) && (peek(c.io.select.ready) != 1)) {
      step(1)
      time += 1
    }
*/
    while ((time < timeout) && (in_idx < n_pixel || out_idx < n_pixel)) {
      
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
      
      if((out_idx < n_pixel) && peek(c.io.img_out.valid)==1) {
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

  def window(img_in: Image, row: Int, col: Int) = {
    val win = Array.fill(c.params.n_tap)(Array.fill(c.params.n_tap)(0.toByte))
    val mid = c.params.n_tap/2
    val r_idx = 0
    val c_idx = 0

    for (i <- 0 until c.params.n_tap) {
      r_idx = row - mid + i
      for (j <- 0 until c.params.n_tap) {
        c_idx = col - mid + j
        if (r_idx >= 0 && r_idx < img_in.h && c_idx >= 0 && c_idx < img_in.w) {
	  win(i)(j) = img_in.data(r_idx * img_in.w + c_idx)
        }
      }
    }
    win
  }

  def sym_fir(vals: Array[Byte], coeff: List[Int]) = {
    val all_coeff = coeff ++ coeff.reverse.tail
    (((vals, all_coeff).zipped.map(byteToInt(_) * _).sum >> 8) & 0xFF).toByte
  }

  def gaussian(img_in: Image) = {
    val coeff = c.params.coeff(c.params)
    val img_gauss = Image(img_in.w, img_in.h, img_in.d)
    for (i <- 0 until img_gauss.h) {
      for (j <- 0 until img_gauss.w) {
  	val win = window(img_in, i, j)
	val row_fir = win.map(sym_fir(_, coeff))
	val pix = sym_fir(row_fir, coeff)
	img_gauss.data(i*img_gauss.w + j) = pix.toByte	
      }
    }
    img_gauss
  } 

  def check_img_out(img_exp: Image) = {
    (img_out.data, img_exp.data).zipped.map(_ == _).reduce(_ && _)
  }

  process(random_img, timeout)
  val img_exp = gaussian(random_img)
  val passed = (time < timeout) && check_img_out(img_exp)
  println("Check %b".format(passed))
  if (passed){
    any_passed = true
  } else {
    all_passed = false
  }
  ok = all_passed && any_passed
}
