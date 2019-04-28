package pplAssignment

object F2015B3A70519P
{
  def product(l1: List[Double], l2: List[Double], sum: Double): Double =        //product of two lists
    {
      l1 match {
        case Nil       => sum;
        case m :: rest => product(rest, l2.tail, sum + m * l2.head);
      }
    }


  def dotProduct(matrix_1: List[List[Double]], matrix_2: List[List[Double]]): Double =     //dotproduct of two matrices
    {

      def dotproduct1(l1: List[List[Double]], l2: List[List[Double]], sum: Double): Double =
        {
          if (l1.isEmpty)
            sum;
          else {
            dotproduct1(l1.tail, l2.tail, sum + product(l1.head, l2.head, 0));
          }
				}
      dotproduct1(matrix_1, matrix_2, 0);
    }


  def shiftright(l1: List[List[Double]]): List[List[Double]] =          //shifts the matrix by one col right
    {
      if (l1.isEmpty)
        Nil;
      else {
        l1.head.tail :: shiftright(l1.tail);
      }
    }


  def convolute_col(Image: List[List[Double]], Kernel: List[List[Double]], image_col: Int,  //convolute horizontally
                    kernel_col: Int): List[Double] =
    {
      if (image_col < kernel_col)
        Nil
      else {
        dotProduct(Kernel, Image) :: convolute_col(shiftright(Image), Kernel, image_col - 1, kernel_col);
      }
    }


  def convolute(Image: List[List[Double]], Kernel: List[List[Double]], imageSize: List[Int], //convolute the whole matrix
                kernelSize: List[Int]): List[List[Double]] =
    {
      if (imageSize.head < kernelSize.head)
        Nil
      else {
        convolute_col(Image, Kernel, imageSize.tail.head, kernelSize.tail.head) :: convolute(Image.tail, Kernel,
          imageSize.head - 1 :: imageSize.tail, kernelSize);
      }
    }


  def activationlist(f: Double => Double, l1: List[Double]): List[Double] =   //apply activation function on a list
    {
      if (l1.isEmpty)
        Nil;
      else {
        f(l1.head) :: activationlist(f, l1.tail);
      }
    }


  def activationLayer(activationFunc: Double => Double, Image: List[List[Double]]): List[List[Double]] =
    {                                                                              //apply activation function on matrix

      if (Image.isEmpty)
        Nil;
      else {
        activationlist(activationFunc, Image.head) :: activationLayer(activationFunc, Image.tail);
      }
		}


  def findmaxlist(l1: List[Double]): Double =          //max in a list
    {
      if (l1.tail.isEmpty) 
			{
        l1.head
      }
      else 
      {
        val temp = findmaxlist(l1.tail);
        if (temp > l1.head)
          temp
        else
          l1.head
      }
    }


  def findminlist(l1: List[Double]): Double =        //min in a list
    {
      if (l1.tail.isEmpty) 
      {
        l1.head
      } 
      else
      {
        val temp = findminlist(l1.tail);
        if (temp < l1.head)
          temp
        else
          l1.head
      }
    }
    

  def findmin(l1: List[List[Double]]): Double =    //min in a matrix
    {
      if (l1.tail.isEmpty) {
        findminlist(l1.head);
      } else {
        val temp = findmin(l1.tail);
        if (temp < findminlist(l1.head))
          temp
        else
          findminlist(l1.head);
      }
		}
    

  def findmax(l1: List[List[Double]]): Double =     //max in a matrix
    {
      if (l1.tail.isEmpty) {
        findmaxlist(l1.head);
      } else {
        val temp = findmax(l1.tail);
        if (temp > findmaxlist(l1.head))
          temp
        else
          findmaxlist(l1.head);
      }
		}


  def scalingfunc(x: Double, min: Double, max: Double): Int =   //function used to normalize
    {
      ((((x - min) / (max - min)) * 255)).round.toInt;
    }


  def normaliselist(l1: List[Double], min: Double, max: Double): List[Int] =   //apply normalization on list
    {
      if (l1.isEmpty)
        Nil;
      else {
        scalingfunc(l1.head, min, max) :: normaliselist(l1.tail, min, max);
      }
    }


  def normalise(Image: List[List[Double]]): List[List[Int]] =     //apply normalization on the matrix
    {
      val min = findmin(Image);
      val max = findmax(Image);
      def normalise1(Image: List[List[Double]], min: Double, max: Double): List[List[Int]] =
        {
          if (Image.isEmpty)
            Nil;
          else {
            normaliselist(Image.head, min, max) :: normalise1(Image.tail, min, max);
          }
        }
      normalise1(Image, min, max);
    }


  def shiftbyK(l1: List[List[Double]], K: Int): List[List[Double]] =   //shift  horizontally by K columns
    {
      if (K == 0 || l1.isEmpty)
        l1;
      else {
        shiftbyK(shiftright(l1), K - 1);
      }
    }

	def length3(l1:List[Double],sum:Int) : Int =
    	{
		    if (l1.isEmpty)
		      sum;
		    else {
		       length3(l1.tail,sum+1);
		    }
		  }

  def length1(l1: List[Double]): Int =          //length of a list
    {
    	
		  length3(l1,0);
    }


  def firstk(l1: List[Double], K: Int): List[Double] = //first k elements into a list (for k col)
    {
      if (K == 0 || l1.isEmpty)
        Nil;
      else {
        l1.head :: firstk(l1.tail, K - 1);
      }
    }


  def listgenerator(Image: List[List[Double]], Krow: Int, Kcol: Int): List[Double] = 
                                                                   //first k lists combined into (for k row) one list
    {
      if (Krow == 0 || Image.isEmpty)
        Nil;
      else {
        firstk(Image.head, Kcol) ::: listgenerator(Image.tail, Krow - 1, Kcol);
      }
		}


  def singlePooling(poolingFunc: List[Double] => Double, Image: List[List[Double]], K: Int): List[Double] =
    {                                                //pooling horizontally           
      val M = length1(Image.head);
      val num = M / K;
      def sp(poolingFunc: List[Double] => Double, Image: List[List[Double]], num: Int, K: Int): List[Double] =
        {
          if (num == 0 /*|| Image.isEmpty*/ )
            Nil;
          else {
            poolingFunc(listgenerator(Image, K, K)) :: sp(poolingFunc, shiftbyK(Image, K), num - 1, K);
          }
        }
      sp(poolingFunc, Image, M / K, K);
    }


  def shiftdownbyK(Image: List[List[Double]], K: Int): List[List[Double]] =
    {                                                   // move down by K rows
      if (K == 0)
        Image;
      else {
        shiftdownbyK(Image.tail, K - 1);
      }
    }

	def length2(l1:List[List[Double]],sum:Int) : Int =
			{
				  if (l1.isEmpty)
				    sum;
				  else {
				    length2(l1.tail,sum+1);

				  }
			}

  def lengthrow(l1: List[List[Double]]): Int =          //row length of a matrix
    {
			
			length2(l1,0);
    }


  def poolingLayer(poolingFunc: List[Double] => Double, Image: List[List[Double]], K: Int): List[List[Double]] =
    {                                       // pooling on entire matrix
      val row = lengthrow(Image);
      val col = length1(Image.head);
      def pl(poolingFunc: List[Double] => Double, Image: List[List[Double]], K: Int, row: Int): List[List[Double]] =
        {
          if (row < K /*|| Image.isEmpty*/ )
            Nil;
          else {
            singlePooling(poolingFunc, Image, K) :: pl(poolingFunc, shiftdownbyK(Image, K), K, row - K);
          }
        }
      pl(poolingFunc, Image, K, row);
    }


  def mixedLayer(Image: List[List[Double]], Kernel: List[List[Double]], imageSize: List[Int], kernelSize: List[Int],
                 activationFunc: Double => Double, poolingFunc: List[Double] => Double, K: Int): List[List[Double]] =
    {
      poolingLayer(poolingFunc, activationLayer(activationFunc, convolute(Image, Kernel, imageSize, kernelSize)), K);
    }


  def Relu(x:Double) : Double =
  {
      if(x>0)
        x;
      else
        0;
  }
  
  
  def LRelu(x:Double) : Double =
  {
      if(x>0)
        x;
      else
        0.5*x;
  }
  
  
  def meanlist(l1 : List[Double]) : Double =         //average of a list
   {
   
      def mean(l1 : List[Double],sum:Double,size:Int) : Double = 
      {
          l1 match 
          {
              case Nil => sum/size;
              case x::rest => mean(rest,x+sum,size+1);
          }
      }
      mean(l1,0,0);
 	}
  
  
 def addlist(l1:List[Double], l2:List[Double]) : List[Double] =  //add elements of a list into a new list containing sum
   {
      if(l1.isEmpty || l2.isEmpty)
        Nil;
      else
      {
          (l1.head + l2.head) :: addlist(l1.tail,l2.tail);
      }
   }
  
  
 def addmat(l1:List[List[Double]],l2:List[List[Double]]) : List[List[Double]] = 
 {                                                    //add two matrices into a new matrix
     if(l1.isEmpty || l2.isEmpty)
        Nil;
      else
      {
          addlist(l1.head,l2.head) :: addmat(l1.tail,l2.tail);
      }
   
 }
  
  
  def assembly(Image: List[List[Double]], imageSize: List[Int], w1: Double, w2: Double, b: Double,
               Kernel1: List[List[Double]], kernelSize1: List[Int], Kernel2: List[List[Double]], kernelSize2: List[Int],
               Kernel3: List[List[Double]], kernelSize3: List[Int], Size: Int): List[List[Int]] =
    {
        val o1 = mixedLayer(Image,Kernel1,imageSize,kernelSize1,Relu,meanlist,Size);
        val o2 = mixedLayer(Image,Kernel2,imageSize,kernelSize2,Relu,meanlist,Size);
        val o3 = activationLayer((x:Double)=>b+x,addmat(activationLayer((x:Double)=>w1*x,o1) , 
            activationLayer((x:Double)=>w2*x,o2)));
        
        val o4 = mixedLayer(o3,Kernel3,lengthrow(o3)::length1(o3.head)::Nil,kernelSize3,LRelu,findmaxlist,Size);
        normalise(o4);
    }
}
