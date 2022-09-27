extractQABits = function(qaBand, bitStart, bitEnd,radix) {
  numBits = bitEnd - bitStart + 1
  qaBits = qaBand$rightShift(bitStart)$mod(radix^numBits)
  return (qaBits)
}

#' Title
#'
#' @param image
#'
#' @return
#' @export
#'
#' @examples
#' TODO refactor radix, cloud and shadow bits...write method for collection and tidyee

ee_l8mask = function(image,collection=1, radix=2,type="clouds&shadows") {
  ## Radix for binary (base 2) data.


  # # Extract the QA band.
  if(collection==1){
    qa <-  image$select('BQA')
    bitStartCloudConfidence = 5
    bitEndCloudConfidence = 6

    bitStartShadowConfidence = 7
    bitEndShadowConfidence = 8
  }
  if(collection ==2){
    qa <-  image$select('QA_PIXEL')
    bitStartCloudConfidence = 8
    bitEndCloudConfidence = 9
    bitStartShadowConfidence = 10
    bitEndShadowConfidence = 11
  }


  # # Function that masks dual QA bits

  # Create a mask for the dual QA bit "Cloud Confidence".

  qaBitsCloudConfidence = extractQABits(qaBand = qa,
                                        bitStart = bitStartCloudConfidence,
                                        bitEnd =  bitEndCloudConfidence,radix = radix)
  # Test for clouds, based on the Cloud Confidence value$
  testCloudConfidence = qaBitsCloudConfidence$gte(2)

  # Create a mask for the dual QA bit "Cloud Shadow Confidence"$

  qaBitsShadowConfidence = extractQABits(qa,
                                         bitStartShadowConfidence,
                                         bitEndShadowConfidence,radix=radix)
  # Test for shadows, based on the Cloud Shadow Confidence value$
  testShadowConfidence = qaBitsShadowConfidence$gte(2)


  # Calculate a composite mask and apply it to the image$
  if(type=="clouds&shadows"){
    maskComposite = (testCloudConfidence$Or(testShadowConfidence))$Not()
  }
  if(type=="clouds"){
    maskComposite <- testCloudConfidence$Not()
  }
  if(type=="shadows") {
    maskComposite <- testShadowConfidence$Not()
  }

  return (image$updateMask(maskComposite))
}
