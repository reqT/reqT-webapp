package shared

/**
  * Created by phiped on 5/3/17.
  */
class functions {

  def dollarMethod(model: String): String = {
    val dollarMethod = "\n val shs = m . entitiesOfType ( Stakeholder ) \n val rs = m . entitiesOfType ( Req ) \n val prioSum = shs . map ( s => m / s / Prio ) . sum " +
      "\n val benefitSum = shs . map ( s => s -> ( m / s ) . collect { case Benefit ( b ) => b }. sum ) . toMap " +
      "\n val normalized = rs . map ( r => r has Benefit ( math . round ( shs . map ( s =>( m / s / Prio ) *( m / s / r / Benefit ) *100.0 / ( benefitSum ( s ) * prioSum ) ) . sum ) . toInt ) ) . toModel " +
      "\n val sum = normalized . collect { case Benefit ( b ) => b }. sum"

    "val m="+model+dollarMethod
  }


}
