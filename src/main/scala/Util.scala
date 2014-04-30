package SIFT

import Chisel._

case class ImageType(width:Int, height:Int, dwidth: Int = 8) {
  def subsample(factor: Int = 1) : ImageType = {
    new ImageType(width >> factor, height >> factor, dwidth)
  }
}

object ImageType {
  def apply(filename: String): ImageType = {
    val img = Image(filename)
    new ImageType(img.w, img.h, img.d)
  }
}

class Pixel extends Bundle {
  val r = UInt(width = 8)
  val g = UInt(width = 8)
  val b = UInt(width = 8)
}

class Coord(it: ImageType) extends Bundle {
  val col = UInt(OUTPUT,width=log2Up(it.width))
  val row = UInt(OUTPUT,width=log2Up(it.height))
  
  override def clone: this.type = {
    try {
      super.clone()
    } catch {
      case e: java.lang.Exception => {
        new Coord(it).asInstanceOf[this.type]
      }
    }
  }
}

class Counter(max: UInt) extends Module {
  val io = new Bundle {
    val en = Bool(INPUT)
    val count = UInt(OUTPUT, max.getWidth)
    val top = Bool(OUTPUT)
  }

  def this(max: Int) = this(UInt(max))

  val x = Reg(init = UInt(0, max.getWidth))

  io.count := x
  io.top := x === max

  when (io.en) {x := Mux(io.top, UInt(0), x + UInt(1))}
}

class ImageCounter(it: ImageType) extends Module {
  val io = new Bundle {
    val en = Bool(INPUT)
    val out = new Coord(it)
    val top = Bool(OUTPUT)
  }

  val col_counter = Module(new Counter(it.width-1))
  val row_counter = Module(new Counter(it.height-1))

  col_counter.io.en := io.en
  row_counter.io.en := io.en & col_counter.io.top

  io.out.col := col_counter.io.count
  io.out.row := row_counter.io.count

  io.top := col_counter.io.top & row_counter.io.top
}

object ShiftRegisterEn {
  def apply[T <: Data](data: T, delay: Int, enable: Bool = Bool(true), 
    use_mem: Boolean = false): T = {

    if(use_mem) {
      val srm = Module(new ShiftRegisterMem[T](data, delay))
      srm.io.in := data
      srm.io.en := enable
      srm.io.out
    } else {
      if (delay == 1) {
        RegEnable(data, enable)
      } else {
        RegEnable(apply(data, delay-1, enable), enable)
      }
    }
  }
}

class ShiftRegisterMem[T <: Data](
  gen: T, delay: Int, enable: Bool = Bool(true)) extends Module {

  val io = new Bundle {
    val in = gen.clone.asInput
    val out = gen.clone.asOutput
    val en = Bool(INPUT)
  }

  //println("Gen: %s\nIn: %s\nOut: %s".format(gen, io.in, io.out))
  val buf = Mem(io.in, delay)
  val ptr = Module(new Counter(delay-1))
  ptr.io.en := io.en
  when(io.en) {
    buf(ptr.io.count) := io.in
  }
  io.out := buf(ptr.io.count)
}

object TapDelayLineEn {
  def apply[T <: Data](data: T, delay: Int, enable: Bool = Bool(true), 
    use_mem: Boolean = false, tap: Int = 1): List[T] = {
    
    if (tap <= 1) List(data)
    else {
      data :: apply(ShiftRegisterEn(data, delay, enable, use_mem),
      delay, enable, use_mem, tap-1)
    }
  }
}
