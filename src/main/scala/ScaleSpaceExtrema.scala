package SIFT

import Chisel._

import scala.io.Source
import java.io.File
import scala.util.Random

case class SSEParams(
  it: ImageType, 
  n_oct: Int = 2,
  n_ext: Int = 2,
  n_tap: Int = 5,
  next_tap: Int = 2,
  //coeff: List[UInt] = StdCoeff.GaussKernel,
  coeff: (SSEParams) => List[Int] = StdCoeff.GaussKernel,
  sigma: Double = 0.8,
  mul_delay: Int = 1,
  sum_delay: Int = 1,
  use_mem: Boolean = true
)

class ScaleSpaceExtrema(val params: SSEParams) extends Module {

  val io = new Bundle {
    val img_in = Decoupled(UInt(width=params.it.dwidth)).flip
    val coord = Valid(new Coord(params.it))
    
    val select = Decoupled(UInt(width=8)).flip
    val img_out = Decoupled(UInt(width=params.it.dwidth))
  }
  
  // Count pixels output and input
  val out_count = Module(new ImageCounter(params.it))
  io.coord.bits := out_count.io.out
  out_count.io.en := io.img_out.fire()

  val in_count = Module(new ImageCounter(params.it))
  in_count.io.en := io.img_in.fire()
  
  // Allow changing source when stream is not in process
  io.select.ready := (
    !io.img_in.valid &
    (in_count.io.out.row === UInt(0)) &
    (in_count.io.out.col === UInt(0)) &
    (out_count.io.out.row === UInt(0)) &
    (out_count.io.out.col === UInt(0)))

  // Just generate a pattern on valid for now
  io.coord.valid := out_count.io.out.col > out_count.io.out.row

  // Latch output image source when allowed
  val select_r = Reg(init = UInt(0,8))
  when (io.select.fire()) {
    select_r := io.select.bits
  }
  
  // For now octaves only operate on grayscale byte images
  val gray_it = new ImageType(params.it.width, params.it.height, 8)
  
  // Create sequence of octaves
  val oct = Range(0, params.n_oct).map(
    i => Module(new Octave(params.copy(it = gray_it.subsample(i)), i))
  )

  // Wire downstream octave ports to "next" image ports of previous octave
  for (i <- 0 until params.n_oct) {
    oct(i).io.select := select_r
    
    if (i != 0) {
      oct(i).io.img_in <> oct(i-1).io.next_img_out
      oct(i).io.img_out <> oct(i-1).io.next_img_in
    }
  }
  
  // Always assert last octave ready to avoid stalling pipeline
  oct(params.n_oct-1).io.next_img_out.ready := Bool(true)
  
  // Wire input image to first octave main image input
  io.img_in.ready := oct(0).io.img_in.ready
  oct(0).io.img_in.valid := io.img_in.valid
  
  if (params.it.dwidth == 24) {
    // Approximate (r+b+g)/3 as (r+b+g)*(16 + 4 + 1)/64
    // Also offset by 4 to allow for colorspace mapping
    val sum = (Cat(UInt("h00"), io.img_in.bits(23,16)) + 
      io.img_in.bits(15,8) + io.img_in.bits(7,0))
    val div = (UInt(16)*sum) + (UInt(4)*sum) + sum
    oct(0).io.img_in.bits := (div >> UInt(6)) + UInt(4)
  } else {
    oct(0).io.img_in.bits := io.img_in.bits
  }

  // Wire output image to first octave main image output
  oct(0).io.img_out.ready := io.img_out.ready
  io.img_out.valid := oct(0).io.img_out.valid
  
  if (params.it.dwidth == 24)
    io.img_out.bits := Fill(3,oct(0).io.img_out.bits)
  else 
    io.img_out.bits := oct(0).io.img_out.bits
}

class SSETester(c: ScaleSpaceExtrema) extends Tester(c, false) {
  val dwidth = c.params.it.dwidth
  val width = c.params.it.width
  val height = c.params.it.height

  val n_byte = dwidth/8
  val n_pixel = width * height
  
  val img_out = Image(width, height, dwidth)
  val img_coord = Image(width, height, 24)

  var time = 0
  
  def process(img_in: Image, select: Int, timeout: Int = 1000) = {
    
    var in_idx = 0
    var out_idx = 0
    
    var triplet = 0
    var pixel = 0
    
    reset()
    step(1)
    
    poke(c.io.img_in.valid, 0)
    poke(c.io.select.bits, select)
    poke(c.io.select.valid, 1)
    step(1)

    while((time < timeout) && (peek(c.io.select.ready) != 1)) {
      step(1)
      time += 1
    }

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

      poke(c.io.select.valid, 0)
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
        val coordpix = if (peek(c.io.coord.valid)==1) 0xFF0000 else 0x808080
        for (j <- 0 until 3) {
          img_coord.data(3*out_idx+j) = ((coordpix >> (8*j)) & 0xFF).toByte
        }

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

case class FileTesterParams(
  ctrl: String, 
  img_in: String,
  img_out: String,
  coord: String
)

class SSEFileTester(c: ScaleSpaceExtrema, ftp: FileTesterParams) 
  extends SSETester(c) {

  var ctrl = List(0x00)

  try {
    val ctrl_file = Source.fromFile(ftp.ctrl)
    ctrl = ctrl_file.getLines().map(_.split(",")).flatten.map(_.toInt).toList
    ctrl_file.close()
  } catch {
    case _ : Throwable => {
      println("File \"" + ftp.ctrl + "\" not found")
    }
  }
  
  println("Ctrl Inputs: " + ctrl)
  val n_ctrl = ctrl.length
  
  val file_img_in = Image(ftp.img_in)
  println("w=" + file_img_in.w + " h=" + file_img_in.h + " d=" + file_img_in.d)

  val timeout = 1000

  var any_passed = false
  var all_passed = true

  for (i <- 0 until n_ctrl) {
    println("Submitting image " + i)

    process(file_img_in, ctrl(i), timeout)

    if (time == timeout) {
      println("Timeout on image " + i)
      all_passed = false
    } else {
      any_passed = true
      
      val img_list = ftp.img_out.split("\\.").toList
      val (img_base, img_ext) = img_list.splitAt(img_list.length-1)
      val img_name = img_base(0) + i + "." + img_ext(0)
      println("Writing " + img_name)
      img_out.write(img_name)
    }
  }

  img_coord.write(ftp.coord)
  
  ok = all_passed && any_passed
}

class SSERandomTester(c: ScaleSpaceExtrema) extends SSETester(c) {
  val rand_img = Image(width, height, dwidth)
  val rng = new scala.util.Random()
  rng.nextBytes(rand_img.data)

  val n_g = c.params.n_ext + 3
  val n_d = c.params.n_ext + 2

  var any_passed = false
  var all_passed = true
  
  val timeout = 1000
  for (select <- 0 until 2 + n_g + n_d) {
    process(rand_img, select, timeout)
    val img_exp = expectedImage(rand_img, select)
    //img_out.write("data/out%d.im8".format(select))
    //img_exp.write("data/exp%d.im8".format(select))
    val passed = (time < timeout) && check_img_out(img_exp)
    println("Check %d: %b".format(select,passed))
    if (passed) {
      any_passed = true
    } else {
      all_passed = false
    }
  }

  ok = all_passed && any_passed
  
  def expectedImage(img_in: Image, select: Int) = {
    if (select==0) {
      img_in
    } else if (select < (2 + n_g)) {
      upsample(gauss(downsample(img_in), select-1))
    } else if (select < (2 + n_g + n_d)) {
      upsample(diff(downsample(img_in), select-n_g-1))
    } else {
      img_in
    }
  }

  // Why doesn't this work?
  def im24ToIm8(im24: Image) = {
    val im8 = Image(im24.w, im24.h, 8)
    for (i <- 0 until im8.h) {
      for (j <- 0 until im8.w) {
        im8.data(i*im8.w + j) = im24.data(i*im8.w + 3*j)
      }
    }
    im8
  }
  
  def bars(w:Int,h:Int,d:Int) = {
    val img = Image(w,h,d) 
    for (i <- 0 until img.h) {
      for (j <- 0 until img.w) {
        img.data(i*img.w + j) = (if ((i%8 < 2) || (j%8 < 2)) 0x00 else 0xFF).toByte
      }
    }
    img
  }

  def downsample(img_in: Image) = {
    val img_ds = Image(img_in.w/2, img_in.h/2, img_in.d)
    for (i <- 0 until img_ds.h) {
      for (j <- 0 until img_ds.w) {
        img_ds.data(i*img_ds.w + j) = img_in.data(2*i*img_in.w + 2*j)
      }
    }
    img_ds
  }
  
  def window(img: Image, row: Int, col: Int) = {
    val win = Array.fill(c.params.n_tap)(Array.fill(c.params.n_tap)(0.toByte))
    val mid = c.params.n_tap/2

    var r_idx = 0
    var c_idx = 0
    
    //println("Window (%d,%d)".format(row,col))
    
    for (i <- 0 until c.params.n_tap) {
      r_idx = row-mid+i
      
      //var strl = " "
      for (j <- 0 until c.params.n_tap) {
        c_idx = col-mid+j
        if (r_idx >= 0 && r_idx < img.h && c_idx >= 0 && c_idx < img.w) {
          win(i)(j) = img.data(r_idx*img.w + c_idx)
        }
        //strl = strl + "%d ".format(win(i)(j))
      }
      //println(strl)
    }
    //println("")
    win
  }
  
  def sym_fir(vals: Array[Byte], coeff: List[Int]) = {
    val all_coeff = coeff ++ coeff.reverse.tail
    (((vals, all_coeff).zipped.map(byteToInt(_) * _).sum >> 8) & 0xFF).toByte
  }

  def byteToInt(b: Byte) = {
    if (b < 0) b.toInt + 256 else b.toInt
  }

  def gauss(img_in: Image, n_gauss: Int = 1): Image = {
    if(n_gauss == 0) {
      img_in
    } else if(n_gauss == 1) {
      //val int_coeff = c.params.coeff.map(_.litValue().toInt)
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
    } else {
      gauss(gauss(img_in, n_gauss-1))
    }
  }
  
  def diff(img_in: Image, n_diff: Int = 1) = {
    val g0 = gauss(img_in, n_diff)
    val g1 = gauss(g0)
    val data : Array[Byte] = (g0.data,g1.data).zipped.map(_ - _).map(_.toByte)
    val img_diff = new Image(img_in.w, img_in.h, img_in.d, data)
    img_diff
  }

  def upsample(img_in: Image) = {
    val img_us = Image(img_in.w*2, img_in.h*2, img_in.d)
    for (i <- 0 until img_us.h) {
      for (j <- 0 until img_us.w) {
        img_us.data(i*img_us.w + j) = img_in.data((i/2)*img_in.w + (j/2))
      }
    }
    img_us
  }

  def check_img_out(exp: Image) = {
    (img_out.data, exp.data).zipped.map(_ == _).reduce(_ && _)
  }
}
