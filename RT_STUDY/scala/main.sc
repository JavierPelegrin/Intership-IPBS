// Ammonite 2.4.0, scala 2.13.5

//import $ivy.`org.scala-lang.modules:scala-parallel-collections_2.13:1.0.3`
//import $file.hello, hello._

//> using scala 2.13
//> using dep org.scala-lang.modules::scala-parallel-collections:1.0.4
//> using file hello.sc

import hello._

import scala.collection.parallel.CollectionConverters._
import scala.collection.mutable.LongMap
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.HashSet


import java.io._ 

//RefineLfqResultsImpl.refineElutionTimeCalibration(masterEntities = null, nRuns = 0)

//val inputFile = "../rt_study/data/clean_hfx_ions.tsv"
val inputFile = "hfx_ions_observ.tsv"
val outputFile = "hfx_ion_pairs_observed_extended.tsv"

var lines = Source.fromFile(inputFile).getLines()

val inputHeader = lines.next().split("\t").toList
println(inputHeader)

val masterFtIndices = (0 to 6)
val nRuns = (inputHeader.length - masterFtIndices.length) / 3
// val nRuns = 15
println("nRuns=" + nRuns)

val masterEntities = new scala.collection.mutable.ArrayBuffer[MasterLcMsEntity]

lines.foreach { line =>
    val values = line.split("\t").toList

    val ionId = values(0).toLong
    val peptideId = values(1).toLong
    val pepSeq = values(2)
    val pepMods = values(3)
    val ionMz = values(4).toDouble
    val ionCharge = values(5).toInt
    val masterRt = values(6).toFloat * 60

    // val ionId = values(7).toLong
    // val peptideId = values(0).toLong
    // val pepSeq = values(1)
    // val pepMods = values(2)
    // val ionMz = values(4).toDouble
    // val ionCharge = values(5).toInt
    // val masterRt = values(6).toFloat * 60

    // println( ionId, peptideId, pepSeq, pepMods, ionMz, ionCharge, masterRt )

    val deepLcMods = if (pepMods.isEmpty) pepMods else {
    val tmpDeepLcMods = pepMods.split("; ").map { pepMod =>
        val modName = pepMod.takeWhile(c => c != ' ')
        val modLocation = pepMod.dropWhile(c => c != '(').drop(1).takeWhile(c => c != ')')

        val modLocationAsInt = if (modLocation.endsWith("N-term")) {
            0
        } else if (modLocation.endsWith("C-term")) {
            -1
        } else {
            modLocation.drop(1).toInt
        }

        s"$modLocationAsInt|$modName"
    } .mkString("|")

    tmpDeepLcMods
    }

    var ftColIdx = masterFtIndices.last + 1

    val lcMsEntityByRunNumber = new LongMap[ISerializableLcMsEntity]()
    val rts = new ArrayBuffer[Float](nRuns)

    var maxIntensity = 0f
    for (nRun <- 1 to nRuns) {
        //println("ftColIdx="+ftColIdx)

        if (values(ftColIdx).nonEmpty) {
            val rt = values(ftColIdx).toFloat * 60
            val calRt = values(ftColIdx + nRuns).toFloat * 60
            val intensity = values(ftColIdx + nRuns * 2).toFloat

            println(peptideId,ftColIdx, nRun , intensity)

        // if (values(ftColIdx +  17).nonEmpty) {
        //     val rt = values(ftColIdx +  17).toFloat * 60
        //     val calRt = values(ftColIdx + nRuns + 18 ).toFloat * 60
        //     val intensity = values(ftColIdx + nRuns * 2 + 19).toFloat

            if (intensity > maxIntensity) {
                maxIntensity = intensity
            }

            val entity = LcMsEntity(ionMz,intensity,rt,calRt)
            lcMsEntityByRunNumber.put(nRun, entity)
            rts += rt
        }

        ftColIdx += 1
        // println("ftColIdx="+ftColIdx)
    }

    val meanRt = rts.sum / rts.length

    masterEntities += MasterLcMsEntity(
        id = ionId,
        pepId = peptideId,
        pepSeq = pepSeq,
        pepMods = deepLcMods,
        mz = ionMz,
        charge = ionCharge,
        intensity = maxIntensity,
        // elutionTime = meanRt,
        elutionTime = masterRt,
        lcMsEntityByRunNumber = lcMsEntityByRunNumber,
        runNumbers = lcMsEntityByRunNumber.keys.map(_.toInt).toList.sorted
    )

    // println( List(pepSeq, pepMods, deepLcMods, ionCharge, masterRt).mkString("\t") )
}

val masterEntityById = new LongMap[MasterLcMsEntity]
masterEntities.foreach { masterEntity =>
  masterEntityById.put(masterEntity.id, masterEntity)
}
println(masterEntities.length)

//1 / 0

val (result, correlatedIons, clusterMapping, antiClusterMapping) = RefineLfqResultsImpl.refineElutionTimeCalibration(masterEntities, 5 , 4)
println(result.length)

var outputHeader = List("ion_id_A","ion_id_B","pep_seq_A","pep_seq_B","mods_A","mods_B","charge_A","charge_B","len_A","len_B","intensity_log_A","intensity_log_B","duration_A","duration_B","rt_avg_dist","rt_stddev")
// var outputHeader = List("ion_id_A","ion_id_B","pep_seq_A","pep_seq_B","mods_A","mods_B","charge_A","charge_B","rt_avg_dist_param","rt_stddev_param")


var writer = new PrintWriter (new File (outputFile)) 
writer.println(outputHeader.mkString("\t"))

correlatedIons.foreach { correlatedIon =>

    val masterIonA: MasterLcMsEntity = masterEntityById(correlatedIon.idA)
    val masterIonB: MasterLcMsEntity = masterEntityById(correlatedIon.idB)

    val durationsA = masterIonA.lcMsEntityByRunNumber.values.map(e => e.getLastElutionTime - e.getFirstElutionTime).toList
    val durationsB = masterIonB.lcMsEntityByRunNumber.values.map(e => e.getLastElutionTime - e.getFirstElutionTime).toList
    // val sumA = 0
    // durationsA.foreach(sumA += _)
    // val meanDurationA = sum(durationsA) / durationsA.length
    val meanDurationA = durationsA.sum / durationsA.length
    // val sumB = 0
    // durationsB.foreach(sumB += _)
    // val meanDurationB = sumB / durationsB.length
    val meanDurationB = durationsB.sum / durationsB.length
    val valuesAsStr = List(
        masterIonA.id,
        masterIonB.id,
        masterIonA.pepSeq,
        masterIonB.pepSeq,
        masterIonA.pepMods,
        masterIonB.pepMods,
        masterIonA.charge,
        masterIonB.charge,

        masterIonA.pepSeq.length,
        masterIonB.pepSeq.length,
        math.log10(masterIonA.intensity),
        math.log10(masterIonB.intensity),
        meanDurationA,
        meanDurationB,

        correlatedIon.rtDiffMean,
        correlatedIon.rtDiffStdDev
    )
    
    writer.println(valuesAsStr.mkString("\t"))
}

writer.close()

/*
val cmftById: Map[Long, CalibratedMasterLcMsEntity] = Map() ++ result.map { cmft => (cmft.id,cmft) }

var outputHeader = inputHeader //List("id")
outputHeader ++= (1 to nRuns).map( nRun => s"pred_rt_run$nRun")
outputHeader ++= (1 to nRuns).map( nRun => s"cal_rt_run$nRun")
outputHeader ++= (1 to nRuns).map( nRun => s"rt_error_run$nRun")

var writer = new PrintWriter (new File ("./calibrated_features.tsv")) 
writer.println(outputHeader.mkString("\t"))

val lines2 = Source.fromFile(inputFile).getLines()
lines2.next() // skip header

lines2.foreach { line =>
    val id = line.split("\t").head.toLong
    //val id = new String(chars).toLong

    val cmftOpt = cmftById.get(id)

    val valuesAsStr = if (cmftOpt.isEmpty) { Array(line)
        //""
        //List(line) ++ (1 to nRuns * 3).map("")
    } else {
        val cmft = cmftOpt.get
        val predRts = cmft.predictedRts.map { n => if (n.isNaN()) "" else n.toString }
        val calRts = cmft.calibratedRts.map { n => if (n.isNaN()) "" else n.toString }
        val rtErrors = cmft.rtErrors.map { n => if (n.isNaN()) "" else n.toString }
        Array(line) ++ predRts ++ calRts ++ rtErrors
    }

    writer.println(valuesAsStr.mkString("\t"))


}

writer.close ()
*/

/*
writer = new PrintWriter (new File ("./data/cluster_mapping2.tsv")) 
writer.println("feature_id\tcorrelated_feature_id")

clusterMapping.toList.sortBy(_._1).foreach { case (feature_id, correlated_feature_ids) =>
    correlated_feature_ids.foreach { correlated_feature_id =>
        writer.println(s"$feature_id\t$correlated_feature_id")
    }
}

writer.close()

writer = new PrintWriter (new File ("./data/anti_cluster_mapping.tsv")) 
writer.println("feature_id\tnon_correlated_feature_id")

antiClusterMapping.toList.sortBy(_._1).foreach { case (feature_id, non_correlated_feature_ids) =>
    non_correlated_feature_ids.foreach { non_correlated_feature_id =>
        writer.println(s"$feature_id\t$non_correlated_feature_id")
    }
}

writer.close()*/