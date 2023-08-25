// Ammonite 2.3.8, scala 2.13.5

import java.util.concurrent.atomic.AtomicLong

import scala.collection.Map
import scala.collection.mutable.HashMap

def test() {
  println("hello")
}

object misc {

  object InMemoryIdGen {

    private val _idSequenceByClassName = new HashMap[String, AtomicLong]()

    protected def getIdSequence(entityClassName: String): AtomicLong = this.synchronized {
      _idSequenceByClassName.getOrElseUpdate(entityClassName, new AtomicLong(0L))
    }

    def getLastGenIdByEntityName(): Map[String,Long] = _idSequenceByClassName.map { case (k,v) => k -> v.get.longValue }

    def setLastGenIdByEntityName(idMapping: Map[String,Long]): Unit = {
      for ((className, lastGenId) <- idMapping) {
        setLastGeneratedId(className, lastGenId)
      }
    }

    def getLastGeneratedId(className: String): Option[Long] = {
      _idSequenceByClassName.get(className).map(_.get.longValue)
    }

    def setLastGeneratedId(className: String, id: Long): Unit = {
      val idSeq = getIdSequence(className)
      val curId = idSeq.get()
      require(id <= curId, "the new ID (negative number) can't be higher than the previous one")

      idSeq.set(id)
    }

  }

  trait InMemoryIdGen {

    private val inMemoryIdSequence = InMemoryIdGen.getIdSequence(this.getClass.getName)

    def generateNewId(): Long = inMemoryIdSequence.decrementAndGet()

  }
}


object MathUtils {

  /** Computes the median value of a sequence of Doubles */
  /*def median(s: Seq[Double]): Double = {
    val (lower, upper) = s.sortWith(_<_).splitAt(s.size / 2)
    if (s.size % 2 == 0) (lower.last + upper.head) / 2.0 else upper.head
  }
  
  def median(s: Seq[Float]): Float = {
    val (lower, upper) = s.sortWith(_<_).splitAt(s.size / 2)
    if (s.size % 2 == 0) (lower.last + upper.head) / 2.0f else upper.head
  }*/

  def isZeroOrNaN( value: Float ) = value.isNaN || value == 0f
  def isZeroOrNaN( value: Double ) = value.isNaN || value == 0d
  
  def filteredMean(s: Array[Float]): Float = {
    val defS = s.filter( isZeroOrNaN(_) == false )
    if( defS.isEmpty ) Float.NaN else defS.sum / defS.length
  }
  
  def filteredMedian(s: Array[Float]): Float = {
    val defS = s.filter( isZeroOrNaN(_) == false )
    if( defS.isEmpty ) Float.NaN else median(defS)
  }
  
  def filteredMean(s: Array[Double]): Double = {
    val defS = s.filter( isZeroOrNaN(_) == false )
    if( defS.isEmpty ) Double.NaN else defS.sum / defS.length
  }
  
  def filteredMedian(s: Array[Double]): Double = {
    val defS = s.filter( isZeroOrNaN(_) == false )
    if( defS.isEmpty ) Double.NaN else median(defS)
  }
  
  def median[T](s: collection.Seq[T])(implicit n: Fractional[T]): T = {
    import n._
    val (lower, upper) = s.sortBy(x=>x).splitAt(s.size / 2)
    if (s.size % 2 == 0) (lower.last + upper.head) / fromInt(2) else upper.head
  }
  
  def getMedianObject[T]( objects: collection.Seq[T], sortingFunc: Function2[T,T,Boolean] ): T = {
  
    val sortedObjects = objects.sortWith { (a,b) => sortingFunc(a,b) } 
    val nbObjects = sortedObjects.length
    
    // Compute median index
    var medianIndex = 0
    
    // If even number of objects
    if( nbObjects % 2 == 0 ) medianIndex = nbObjects / 2
    // Else => odd number of objects
    else medianIndex = (nbObjects-1) / 2
    
    sortedObjects(medianIndex)
    
  }  
  
  /*object factorizeSeq {
    def apply (seq : Seq[_]) : Array[Pair[_, _]] = {
      val p = new scala.collection.mutable.ArrayBuffer[Pair[_, _]]()
      for (e1 <- seq) for (e2 <- seq) p += Pair(e1, e2)
      p.toArray
    }
  }*/
  
  object combinations {
 
    def apply[A](n: Int, ls: List[A]): List[List[A]] =
      if (n == 0) List(Nil)
      else flatMapSublists(ls) { sl =>
        apply(n - 1, sl.tail) map {sl.head :: _}
      }
    
    // flatMapSublists is like list.flatMap, but instead of passing each element
    // to the function, it passes successive sublists of L.
    private def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] = 
      ls match {
        case Nil => Nil
        case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
      }
  
  }
  
  /** Compute slope and intercept of a line using two data points coordinates */
  def calcLineParams( x1: Double, y1: Double, x2: Double, y2: Double ): Tuple2[Double,Double] =  {
    
    val deltaX = x2 - x1
    require( deltaX != 0, "can't solve line parameters with two identical x values (" + x1 + ")" )
    
    val slope = (y2 - y1) / deltaX
    val intercept = y1 - (slope * x1)
    
    ( slope, intercept )
  }
  
  /** 
   * Interpolates a value using the slope and intercept of a line
   * estimated with two consecutive data points coordinates
   * in the provided Tupple2[Double,Double] vector.
   * Assumes that XY values are already sorted by X.
   * If fixOutOfRange is true then out of range values will be replaced by first or last XY pair of the vector.
   *  
   * @param xValue the X value
   * @param xyValues the XY values
   * @paramm fixOutOfRange enable/disable the support for out of range values
   * @return the interpolated Y value
   */
  // TODO: create a LinearInterpolator class which allows to index the XY vector (faster lookup) => use EntityHistogram as backend ???
  def linearInterpolation( xValue: Double, xyValues: collection.Seq[(Double,Double)], fixOutOfRange: Boolean ): Double = {
    
    var index = xyValues.indexWhere( _._1 >= xValue )
    if( index == -1 ) {
      if( !fixOutOfRange ) throw new IllegalArgumentException("index is out of range")
      else index = if( xValue < xyValues.head._1 ) 0 else -1
    }
    
    // If we are looking at the left-side of the vector boundaries
    // then we take the Y value of the first element
    if( index == 0  ) xyValues.head._2
    // Else if we are looking at the right-side of the vector boundaries
    // then we take the Y of the last element
    else if( index == -1 ) xyValues.last._2
    // Else we are inside the vector boundaries
    // We then compute the linear interpolation
    else {
      val( x1, y1 ) = xyValues(index-1)
      val( x2, y2) = xyValues(index)
      
      // If the vector contains two consecutive values with a same X coordinate
      // Then we take the mean of the corresponding Y values
      if( x1 == x2 ) (y1 + y2)/2
      // Else we compute the linear interpolation
      else {
        val ( a, b ) = calcLineParams( x1, y1, x2, y2 )
        (a * xValue + b)
      }
    }
 
  }
  
  def linearInterpolation( xValue: Double, xyValues: collection.Seq[(Double,Double)] ): Double = {
    linearInterpolation(xValue,xyValues,true)
  }
  
  /** 
   * Interpolates a value using the slope and intercept of a line
   * estimated with two consecutive data points coordinates
   * in the provided Pair[Float,Float] vector.
   * Assumes that XY values are already sorted by X.
   * If fixOutOfRange is true then out of range values will be replaced by first or last XY pair of the vector.
   *  
   * @param xValue the X value
   * @param xyValues the XY values
   * @paramm fixOutOfRange enable/disable the support for out of range values
   * @return the interpolated Y value
   */
  def linearInterpolation( xValue: Float, xyValues: collection.Seq[(Float,Float)], fixOutOfRange: Boolean ): Float = {
    
    var index = xyValues.indexWhere( _._1 >= xValue )
    if( index == -1 ) {
      if( !fixOutOfRange ) throw new IllegalArgumentException("index is out of range")
      else index = if( xValue < xyValues.head._1 ) 0 else -1
    }
    
    // If we are looking at the left-side of the vector boundaries
    // then we take the Y value of the first element
    if( index == 0 ) xyValues.head._2
    // Else if we are looking at the right-side of the vector boundaries
    // then we take the Y of the last element
    else if( index == -1 ) xyValues.last._2
    // Else we are inside the vector boundaries
    // We then compute the linear interpolation
    else {
      val( x1, y1 ) = xyValues(index-1)
      val( x2, y2) = xyValues(index)
      
      // If the vector contains two consecutive values with a same X coordinate
      // Then we take the mean of the corresponding Y values
      if( x1 == x2 ) (y1 + y2)/2
      // Else we compute the linear interpolation
      else {
        val ( a, b ) = calcLineParams( x1, y1, x2, y2 )
        (a * xValue + b).toFloat
      }
    }
 
  }
  
  def linearInterpolation( xValue: Float, xyValues: collection.Seq[(Float,Float)] ): Float = {
    linearInterpolation(xValue,xyValues,true)
  }
  
}

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LongMap

import MathUtils.{filteredMedian, isZeroOrNaN}
import misc.InMemoryIdGen

trait ILcMsEntity extends Any {
  def id: Long
  def mz: Double
  def getApexIntensity(): Float
  def getApexElutionTime(): Float
  def getFirstElutionTime(): Float
  def getLastElutionTime(): Float
  def getWeightedAverageTime(): Float
  def getCalibratedTime(): Option[Float]

  def getCalibratedTimeOrRawTime(): Float = getCalibratedTime().getOrElse(this.getApexElutionTime)
}

trait ISerializableLcMsEntity extends AnyRef with ILcMsEntity

object LcMsEntity extends InMemoryIdGen {
  def apply(masterMz: Double, intensity: Float, rt: Float, calibratedRt: Float): LcMsEntity = {
    LcMsEntity(LcMsEntity.generateNewId(), masterMz, intensity, rt, rt, rt, rt, Some(calibratedRt))
  }
}

case class LcMsEntity(
  id : Long,
  mz: Double,
  apexIntensity: Float,
  apexElutionTime: Float,
  firstElutionTime: Float,
  lastELutionTime: Float,
  weightedAverageTime: Float,
  calibratedTime: Option[Float]
) extends ISerializableLcMsEntity {

  def getApexIntensity(): Float = apexIntensity
  def getApexElutionTime(): Float = apexElutionTime
  def getFirstElutionTime(): Float = firstElutionTime
  def getLastElutionTime(): Float = lastELutionTime
  def getWeightedAverageTime(): Float = weightedAverageTime
  def getCalibratedTime(): Option[Float] = calibratedTime
}


object MasterLcMsEntity extends InMemoryIdGen

case class MasterLcMsEntity(
  id: Long,
  pepId: Long,
  pepSeq: String,
  pepMods: String,
  mz: Double,
  charge: Float,
  intensity: Float,
  elutionTime: Float,
  lcMsEntityByRunNumber: scala.collection.mutable.LongMap[ISerializableLcMsEntity],
  runNumbers: collection.Seq[Int]
) {
  var isLandmark: Boolean = false

  //val predictedRtByRunNumber: LongMap[Float] = new LongMap[Float]()
  //predictedRtByRunNumber.sizeHint(runNumbers)

  def getTimeInRun(runNumber: Int): Option[Float] = {
    this.lcMsEntityByRunNumber.get(runNumber).map(_.getApexElutionTime())
  }

  def getCalibratedTimeInRun(runNumber: Int): Option[Float] = {
    this.lcMsEntityByRunNumber.get(runNumber).map(_.getCalibratedTimeOrRawTime())
  }
}
case class CorrelatedRtRow(
  values: Array[Float],
  stdDev: Double
)
case class CorrelatedMasterLcMsEntity(
  idA: Long, // MasterLcMsEntity.id
  idB: Long, // MasterLcMsEntity.id
  rtDiffMean: Double,
  rtDiffStdDev: Double
)

/*case class CalibratedLcMsEntity(
  id: Long,
  calibratedRt: Float,
  predictedRt: Float
)*/

case class CalibratedMasterLcMsEntity(
  id: Long,
  entityIds: Array[Long],
  calibratedRts: Array[Float],
  predictedRts: Array[Float],
  rtErrors: Array[Float]
)

object RefineLfqResultsImpl {

  // Apply the CARPET algorithm here
  def refineElutionTimeCalibration(
    masterEntities: collection.Seq[MasterLcMsEntity],
    nRuns: Int,
    refRunNumber: Int
  ): (
      collection.Seq[CalibratedMasterLcMsEntity],
      collection.Seq[CorrelatedMasterLcMsEntity],
      LongMap[ArrayBuffer[Long]],
      LongMap[ArrayBuffer[Long]]
    ) = {

    val refRunIdx = refRunNumber - 1
    val runsIndices = (0 until nRuns).toArray

    // TODO: adjust this value automatically (crucial parameter)
    val slidingSize = 400
    val halfSlidingSize = slidingSize / 2
    //println("halfSlidingSize: " + halfSlidingSize)

    val consecutiveProximityMapsSize = 10
    val consecutiveProximityMaps = new ArrayBuffer[LongMap[collection.Seq[Int]]](consecutiveProximityMapsSize)
    var consecutiveProximityMapsIndex = 0
    var isConsecutiveProximityMapsFull = false

    def addProximityMap(proximityMap: LongMap[collection.Seq[Int]]): Unit = {
      if (isConsecutiveProximityMapsFull)
        consecutiveProximityMaps(consecutiveProximityMapsIndex) = proximityMap
      else {
        consecutiveProximityMaps += proximityMap
      }

      consecutiveProximityMapsIndex += 1
      if (consecutiveProximityMapsIndex >= consecutiveProximityMapsSize) {
        consecutiveProximityMapsIndex = 0
        isConsecutiveProximityMapsFull = true
      }
    }

    def computeAverageProximityMap(): LongMap[collection.Seq[Int]] = {
      // TODO: aggregates values by determining the highest occurencies
      /*for (proximityMap <- consecutiveProximityMaps) {
        for ((refCol,otherCols) <- proximityMap) {
          otherCols
        }
      }*/

      // For now we use a simple method => just take the most populated proximityMap
      if (consecutiveProximityMaps.isEmpty) LongMap.empty[collection.Seq[Int]]
      else if (consecutiveProximityMaps.length == 1) consecutiveProximityMaps.head
      else {
        consecutiveProximityMaps.maxBy { proximityMap =>
          proximityMap.map(_._2.length).sum
        }
      }
    }

    val calibratedMasterLcMsEntities = new ArrayBuffer[CalibratedMasterLcMsEntity](masterEntities.length)
    val correlatedIons = new ArrayBuffer[CorrelatedMasterLcMsEntity](masterEntities.length)
    val corrMLEIdsByMLEId = new LongMap[ArrayBuffer[Long]]
    val nonCorrMLEIdsByMLEId = new LongMap[ArrayBuffer[Long]]

    // Use only entities having at least two observed values and sort them by elution time
    // We finally define sliding group of co-eluting entities
    var currentIonIdx = -1
    masterEntities.filter(_.runNumbers.length >= 2).sortBy(_.elutionTime).sliding(slidingSize,1) foreach { coElutingMasterEntities =>

      currentIonIdx += 1
      val currentIon = coElutingMasterEntities(halfSlidingSize)

      // Exclude ion if it is already a landmark (TODO: do we really need this?)
      if (!currentIon.isLandmark) {

        // TODO: test all RTs
        //val curIonId = currentIon.id
        val observedRunNumbers = currentIon.runNumbers
        val curEntityByRunNum = currentIon.lcMsEntityByRunNumber

        val relatedIons = coElutingMasterEntities.filter(_ != currentIon)
        val relatedIonsCount = relatedIons.length

        val corrRtsMatrix = new ArrayBuffer[CorrelatedRtRow](relatedIonsCount)
        //val allRtsMatrix = new ArrayBuffer[Array[Float]](relatedIonsCount)

        relatedIons.map { relatedIon =>

          val relatedLcMsEntityByRunNum = relatedIon.lcMsEntityByRunNumber

          //println("relatedIonRecord id=" + relatedIonRecord.getInt(0))

          // Fill allRtMatrix with data
          /*val rtRow = Array.fill[Float](nRuns)(Float.NaN)
          runsIndices foreach { runIdx =>
            //rtRow(runIdx) = relatedIon.getCalibratedTimeInRun(runNum).getOrElse(Float.NaN)
            rtRow(runIdx) = relatedIon.getTimeInRun(runNum).getOrElse(Float.NaN)
          }
          allRtsMatrix += rtRow*/

          // TODO: filter using PSM information
          val relatedIonRunNumbers = relatedIon.runNumbers
          val intersectedRunNumbers = observedRunNumbers.intersect(relatedIonRunNumbers).sorted.toArray

          if (intersectedRunNumbers.length < 2) None
          else {

            val curIonRTs = intersectedRunNumbers.map(curEntityByRunNum(_).getApexElutionTime())
            val relatedIonRTs = intersectedRunNumbers.map(relatedLcMsEntityByRunNum(_).getApexElutionTime())

            // Compute RT differences
            val rtDiffs = curIonRTs.zip(relatedIonRTs).filterNot { case (x, y) =>
              isZeroOrNaN(x) || isZeroOrNaN(y)
            } map { case (x, y) =>
              y - x
            }

            val mean = rtDiffs.sum.toDouble / rtDiffs.length
            //println("mean="+mean)

            // TODO: use StatUtils
            val stdDev = Math.sqrt(rtDiffs.map(_ - mean).map(t => t * t).sum / rtDiffs.length)
            val cv = if (mean == 0.0) stdDev else 100.0 * stdDev / math.abs(mean)
            //println("stdDev="+stdDev)

            if (stdDev < 20) { // TODO: configure me
              val corrRtRow = Array.fill[Float](nRuns)(Float.NaN)
              runsIndices.foreach { runIdx =>
                corrRtRow(runIdx) = relatedIon.getTimeInRun(runIdx + 1).getOrElse(Float.NaN)
              }
              corrRtsMatrix += CorrelatedRtRow(corrRtRow, stdDev)

              corrMLEIdsByMLEId.getOrElseUpdate(currentIon.id, new ArrayBuffer[Long]) += relatedIon.id
            } else {
              nonCorrMLEIdsByMLEId.getOrElseUpdate(currentIon.id, new ArrayBuffer[Long]) += relatedIon.id
            }

            correlatedIons += CorrelatedMasterLcMsEntity(currentIon.id, relatedIon.id, mean, stdDev)
          }

        } // ends relatedIons.map { relatedIon =>

        val refRtValues = (1 to nRuns).map(currentIon.getTimeInRun(_).getOrElse(Float.NaN)).toArray
        //val firstRt = refRtValues.head
        //val refRtMedian = filteredMedian(refRtValues)

        /*def fitRtValues(rtMatrix: Array[Array[Float]]): Seq[Float] = {
          val fittedValues = RatioFitting.fitWithoutImputation(rtMatrix)
          val nf = filteredMedian(refRtValues) / filteredMedian(fittedValues)
          val scaledFittedValues = (0 to nRuns - 1).map(fittedValues(_) * nf)

          scaledFittedValues
        }*/

        /*
        // FIXME: we should use a mathematical error minimization similar to RatioFitting.fitWithoutImputation()
        def fitRtValues(rtMatrix: Array[Array[Float]]): Seq[Float] = {
          val scaledRtMatrix = rtMatrix.map { rtRow =>
            val nf = refRtMedian / filteredMedian(rtRow)
            val scaledFittedValues = rtRow.map(_ * nf)
            scaledFittedValues
          }

          scaledRtMatrix.transpose.map(c => filteredMedian(c))
        }*/

        /*if (!isZeroOrNaN(firstRt) && byMs2QcIdSet.contains(1) && allRtsMatrix.nonEmpty) {
          val avgFittedRtValues = allRtsMatrix.toArray.transpose.map { colValues =>
            filteredMedian(colValues)
          }
          //val avgFittedRtValues = fitRtValues(allRtMatrix.toArray)
          var i = 0
          val fixedRts = refRtValues.zip(avgFittedRtValues).map { case(refRT,fittedRT) =>
            i += 1
            if (i == 1) refRT
            if (isZeroOrNaN(refRT) || isZeroOrNaN(fittedRT)) null
            else firstRt + (fittedRT - refRT)
          }

          fixedRtMatrix += Row.fromSeq(Seq(curIonId) ++ fixedRts)
        }*/

        if (corrRtsMatrix.isEmpty) {
          //println("No correlated ions found for ion with id=" + curIonId)
          //println(currentIon.mz, currentIon.elutionTime)
        }
        else { 
          // if (corrRtMatrix.length == 1)
          //println("corrRtsMatrix length = " + corrRtsMatrix.length)
          //println("ref: "+refRtValues.mkString("\t"))

          /*val definedQcMap = new collection.mutable.LongMap[Int](10)
          var definedQcSet = new collection.mutable.HashSet[Int]()*/

          val filteredCorrRtMatrix = corrRtsMatrix.sortBy(_.stdDev).map(_.values).takeWhile { row =>
            // FIXME: filter me
            /*val allQcsNotYetDefined = definedQcSet.size < 8
            val rtList = row.toArray
            for (i <- 0 to 7; if !isZeroOrNaN(rtList(i))) {
              val defQcs = definedQcMap.getOrElse(i,0) + 1
              definedQcMap.update(i,defQcs)
              if (defQcs >= 3) definedQcSet += i
            }
            allQcsNotYetDefined*/

            true
          }

          /*if (currentIonIdx == 1422) {
            println(filteredCorrRtMatrix.map(_.toList))
          }*/

          // *** Compute pairwise RT distance between matrix cols *** //
          val transposedCorrRtMatrix = filteredCorrRtMatrix.transpose
          val indexedCols = transposedCorrRtMatrix.zipWithIndex

          //type IndexedCol = (ArrayBuffer[Float], Int)
          val pairwiseRtDistances = new LongMap[collection.Seq[(Float, Int)]]()

          for (col1 <- indexedCols) {
            val col1Idx = col1._2

            val rtDists = new ArrayBuffer[(Float, Int)](indexedCols.length)
            for (col2 <- indexedCols; if col1 != col2) {

              val col2Idx = col2._2
              //val idxPair = if (col1Idx < col2Idx) col1Idx -> col2Idx else col2Idx -> col1Idx

              val pairwiseAbsRtDiffs = col2._1.zip(col1._1).map { case (v1,v2) => math.abs(v2 - v1) } filter(!_.isNaN)
              //assert(pairwiseAbsRtDiffs.length >= 2, "not enough shared ion RTs between runs")

              // We exclude the current column if we don't have at least 2 correlated retention times
              if (pairwiseAbsRtDiffs.length >= 3) {
                val avgAbsRtDiff = pairwiseAbsRtDiffs.sum / pairwiseAbsRtDiffs.length

                rtDists += Tuple2(avgAbsRtDiff, col2Idx)
              }
            }

            pairwiseRtDistances.put(col1Idx, rtDists)
          }

          // *** Determine for each column (run) the sorted list of closest columns (runs) *** //
          val rtProximityMap: LongMap[collection.Seq[Int]] = pairwiseRtDistances.map { case (colIdx,colDists) =>
            colIdx -> colDists.sortBy(_._1).map(_._2)
          }

          addProximityMap(rtProximityMap)
          val avgProximityMap = computeAverageProximityMap()

          //println("currentIon[" +currentIonIdx + "] -> "+ avgProximityMap)

          // *** Predict retention time according to the closest run *** //
          val predictedRts = for (runIdx <- runsIndices) yield {
            val colIdx = runIdx
            //val refRt = refRtValues(colIdx)

            // Retrieve the 3 nearest runs using the computed RT proximity mapping
            val closestCols = avgProximityMap(colIdx)
            val closestColsIndices = closestCols.filter { colIdx =>
              !refRtValues(colIdx).isNaN
            } take(3) // TODO: configure this value? or use a regression instead?

            val predictedRt = if (closestColsIndices.isEmpty) Float.NaN
            else {
              // Compute the median of RT predictions
              val closestPredictedRts = closestColsIndices.toArray.map { closestColIdx =>

                val curCorrRts = transposedCorrRtMatrix(colIdx)
                val closestCorrRts = transposedCorrRtMatrix(closestColIdx)
                val rtDiffs = curCorrRts.zip(closestCorrRts).map { case (curRt,corrRt) =>
                  curRt - corrRt
                } filter(!_.isNaN)

                //assert(rtDiffs.nonEmpty, "not enough ion shared RTs")
                if (rtDiffs.isEmpty) Float.NaN
                else {
                  val avgRtDiff = rtDiffs.sum / rtDiffs.length
                  refRtValues(closestColIdx) + avgRtDiff
                }
              }

              filteredMedian(closestPredictedRts)
            }

            predictedRt
          }

          val calibratedRts = runsIndices.map { runIdx =>
            val colIdx = runIdx

            if (runIdx + 1 == refRunNumber) {
              refRtValues(colIdx)
            } else {
              val rt = refRtValues(colIdx)
              if (rt.isNaN) rt
              else {
                val predictedRt = predictedRts(colIdx)
                var predictedRefRt = predictedRts(refRunIdx)
                if (predictedRefRt.isNaN) predictedRefRt = refRtValues(refRunIdx)
                val rtShift = predictedRefRt - predictedRt
                val calibratedRt = rt + rtShift
                calibratedRt
              }
            }
          }

          val rtErrors = refRtValues.zip(predictedRts).map { case (rt,predRt) =>
            predRt - rt
          }

          val lcMsEntityByRunNumber = currentIon.lcMsEntityByRunNumber
          calibratedMasterLcMsEntities += CalibratedMasterLcMsEntity(
            id = currentIon.id,
            entityIds = runsIndices.map(runIdx => lcMsEntityByRunNumber.get(runIdx + 1).map(_.id).getOrElse(0L)),
            calibratedRts = calibratedRts,
            predictedRts = predictedRts,
            rtErrors = rtErrors
          )
          //println(rtErrors.toList)

          /*val calibratedRtsAsStr = calibratedRts.zip(rtPredictions).map { case (calibRt,(rt,predRt)) =>
            if (calibRt.isNaN) "NaN"
            else {
              val rtError = predRt - rt
              val rtErrorAsStr = if (rtError.isNaN) "" else if (rtError > 0) s"+${rtError.toInt}" else rtError.toInt.toString

              s"$calibRt$rtErrorAsStr"
            }
          }

          println(refRtValues.mkString("\t")+"\t"+calibratedRtsAsStr.mkString("\t"))*/

          //println(refRtValues.mkString("\t")+"\t"+rtPredictions.mkString("\t"))

          /*val rtPredictionErrors = rtPredictions.map { case (rt,predictedRt) =>
            val rtError = predictedRt - rt
            if (rtError.isNaN) "" else "%.2f".format(rtError)
          }
          println(rtPredictionErrors.mkString("\t"))*/

          //val scaledFittedValues = fitRtValues(filteredCorrRtMatrix.toArray)
          //println(refRtValues.mkString("\t")+"\t"+scaledFittedValues.mkString("\t"))

          //if (refRtValues.head > 4000) System.exit(1)
        }


      }
    }

    (calibratedMasterLcMsEntities, correlatedIons, corrMLEIdsByMLEId, nonCorrMLEIdsByMLEId)
  }
}