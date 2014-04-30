package SIFT

import Chisel._

object SIFT {
  def main(args: Array[String]): Unit = {
    val tutArgs = args.slice(1, args.length)
    val res =

    // Random_width_n-ext_n-tap
    // Assuming 4x3 aspect ratio for grayscale image, and one octave
    if (args(0).slice(0,11) == "DownSampler") {
      val parts = args(0).split("_")
      val width = parts(1).toInt
      val n_ext = parts(2).toInt
      val n_tap = parts(3).toInt
      val use_mem = !((parts.length > 4) && (parts(4) == "r"))

      val params = SSEParams(
        it = new ImageType(width,3*width/4,8),
        n_ext = n_ext,
        n_tap = n_tap,
        n_oct = 1,
        use_mem = use_mem
      )
      
      println(params.toString)

      chiselMainTest(tutArgs, () => Module(new DownSampler(params)))
        {c => new DownSamplerTester(c) } 
    }
    if (args(0).slice(0,9) == "UpSampler") {
      val parts = args(0).split("_")
      val width = parts(1).toInt
      val n_ext = parts(2).toInt
      val n_tap = parts(3).toInt
      val use_mem = !((parts.length > 4) && (parts(4) == "r"))

      val params = SSEParams(
        it = new ImageType(width,3*width/4,8),
        n_ext = n_ext,
        n_tap = n_tap,
        n_oct = 1,
        use_mem = use_mem
      )
      
      println(params.toString)

      chiselMainTest(tutArgs, () => Module(new UpSampler(params)))
        {c => new UpSamplerTester(c) } 
    }
    if (args(0).slice(0,8) == "Gaussian") {
      val parts = args(0).split("_")
      val width = parts(1).toInt
      val n_ext = parts(2).toInt
      val n_tap = parts(3).toInt
      val use_mem = !((parts.length > 4) && (parts(4) == "r"))

      val params = SSEParams(
        it = new ImageType(width,3*width/4,8),
        n_ext = n_ext,
        n_tap = n_tap,
        n_oct = 1,
        use_mem = use_mem
      )
      
      println(params.toString)

      chiselMainTest(tutArgs, () => Module(new Gaussian(params)))
        {c => new GaussianTester(c) } 
    }

    if (args(0).slice(0,6) == "Random") {
      val parts = args(0).split("_")
      val width = parts(1).toInt
      val n_ext = parts(2).toInt
      val n_tap = parts(3).toInt
      val use_mem = !((parts.length > 4) && (parts(4) == "r"))

      val params = SSEParams(
        it = new ImageType(width,3*width/4,8),
        n_ext = n_ext,
        n_tap = n_tap,
        n_oct = 1,
        use_mem = use_mem
      )
      
      println(params.toString)

      chiselMainTest(tutArgs, () => Module(new ScaleSpaceExtrema(params)))
        {c => new SSERandomTester(c) } 
    } else {
      args(0) match {
        case "ScaleSpaceExtrema" => {
          val params = SSEParams(
            it = ImageType("data/in.im24"),
            n_oct = 1
          )

          val ftp = FileTesterParams(
            "data/control.csv",
            "data/in.im24",
            "data/out.im24",
            "data/coord.im24"
          )

          chiselMainTest(tutArgs, () => Module(new ScaleSpaceExtrema(params)))
            {c => new SSEFileTester(c, ftp)}
        }

        case "Debug" => {
          val params = SSEParams(
            it = ImageType("data/count.im8"),
            n_oct = 1,
            coeff = StdCoeff.CenterKernel
          )

          val ftp = FileTesterParams(
            "data/debug.csv",
            "data/count.im8",
            "data/debug.im8",
            "data/debug_coord.im24"
          )

          chiselMainTest(tutArgs, () => Module(new ScaleSpaceExtrema(params)))
            {c => new SSEFileTester(c, ftp)}
        }

        // Only for generating verilog to hook in to Zedboard Xillydemo system
        case "Zedboard" => {
          val params = SSEParams(
            it = new ImageType(640,480,24),
            n_oct = 1
          )

          chiselMain(tutArgs, () => Module(new ScaleSpaceExtrema(params)))
        }
        /*
        case "test-gaussian" => {
          chiselMainTest(tutArgs, () => Module(new Gaussian(params)))
            {c => new GaussianTester(c)}
        }*/
      }
    }
  }
}
