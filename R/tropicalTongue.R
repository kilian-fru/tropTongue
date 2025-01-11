#' Estimate the proboscis length of tropical bees
#'
#' @param fam A character vector containing a combination of Apidae, Andrenidae, Halictidae, Megachilidae or Melittidae
#' @param tribe A character vector of the same length containing the tribes
#' @param genus A character vector of the same length containing the genera
#' @param IT_mm A numeric vector containing the intertegular distane in mm
#'
#' @return A numeric vector
#' @export
#'
#' @import BeeIT
#'
#' @examples
#' tropicalTongue("Apidae", "Meliponini", "Trigona", 0.7)
#'
tropicalTongue <- function(fam, tribe, genus, IT_mm){

  genus_mel <- c("Dolichotrigona", "Melipona", "Nannotrigona", "Nogueirapis",
                 "Nannotrigona", "Oxytrigona", "Paratrigona", "Partamona",
                 "Plebeia", "Ptilotrigona", "Scaptotrigona", "Scaura", "Tetragona",
                 "Tetragonisca", "Trigona", "Trigonisca")

  genus_aug <- c("Augochlora", "Augochlorella", "Augochloropsis", "Corynura")

  genus_eug <- c("Eufriesa", "Euglossa", "Eulaema", "Exarete")

  res <- c()

  if(!is.character(fam)){
    stop("fam must be of Type character")
  }

  if(!is.character(tribe)){
    stop("tribe must be of Type character")
  }

  if(!is.character(genus)){
    stop("genus must be of Type character")
  }

  if(!is.numeric(IT_mm)){
    stop("IT_mm must be numeric")
  }

  for(i in 1:length(genus)){

##### Meliponini #####
    if(fam[i]=="Apidae" & tribe[i]=="Meliponini"){

      # Dolichotrigona
      if(genus[i]==genus_mel[1]){

        res[i] <- exp(0.21 + 0.55 * log(IT_mm[i]))

        # Melipona
      } else if(genus[i]==genus_mel[2]){

        res[i] <- exp(1.01 + 0.55 * log(IT_mm[i]))

        # Nannotrigona
      } else if (genus[i]==genus_mel[3]){

        res[i] <- exp(0.36 + 0.55 * log(IT_mm[i]))

        # Nogueirapis
      } else if (genus[i]==genus_mel[4]){

        res[i] <- exp(0.67 + 0.55 * log(IT_mm[i]))

        # Oxytrigona
      } else if (genus[i]==genus_mel[5]){

        res[i] <- exp(0.39 + 0.55 * log(IT_mm[i]))

        # Paratrigona
      } else if (genus[i]==genus_mel[6]){

        res[i] <- exp(0.36 + 0.55 * log(IT_mm[i]))

        # Partamona
      } else if (genus[i]==genus_mel[7]){

        res[i] <- exp(0.94 + 0.55 * log(IT_mm[i]))

        # Plebeia
      } else if (genus[i]==genus_mel[8]){

        res[i] <- exp(0.22 + 0.55 * log(IT_mm[i]))

        # Ptilotrigona
      } else if (genus[i]==genus_mel[9]){

        res[i] <- exp(0.62 + 0.55 * log(IT_mm[i]))

        # Scaptotrigona
      } else if (genus[i]==genus_mel[10]){

        res[i] <- exp(0.81 + 0.55 * log(IT_mm[i]))

        # Scaura
      } else if (genus[i]==genus_mel[11]){

        res[i] <- exp(0.16 + 0.55 * log(IT_mm[i]))

        # Tetragona
      } else if (genus[i]==genus_mel[12]){

        res[i] <- exp(0.21 + 0.55 * log(IT_mm[i]))

        # Tetragonisca
      } else if (genus[i]==genus_mel[13]){

        res[i] <- exp(0.42 + 0.55 * log(IT_mm[i]))

        # Trigona
      } else if (genus[i]==genus_mel[14]){

        res[i] <- exp(0.71 + 0.55 * log(IT_mm[i]))

        # Trigonisca
      } else if (genus[i]==genus_mel[15]){

        res[i] <- exp(0.13 + 0.55 * log(IT_mm[i]))

        # Meliponini general
      } else if (!(genus[i] %in% genus_mel)){

      res[i] <- exp(0.28 + 1.39 * log(IT_mm[i]))

      }

##### Augochlorini #####
    } else if(fam[i]=="Halictidae" & tribe[i]=="Augochlorini"){

      #Augochlora
      if (genus[i]==genus_aug[1]){

        res[i] <- exp(0.50 + 1.23 * log(IT_mm[i]))

        # Augochlorella
      }  else if (genus[i]==genus_aug[2]){

        res[i] <- exp(0.46 + 0.52 * log(IT_mm[i]))

        # Augochloropsis
      } else if (genus[i]==genus_aug[3]){

        res[i] <- exp(0.42 + 0.47 * log(IT_mm[i]))

        # Corynura
      } else if (genus[i]==genus_aug[4]){

        res[i] <- exp(0.09 + 0.11 * log(IT_mm[i]))

        # Augochlorini general
      } else if (!(genus[i] %in% genus_aug)){

      res[i] <- exp(0.38 + 0.64 * log(IT_mm[i]))

      }

##### Euglossini #####
    } else if(fam[i]=="Apidae" & tribe[i]=="Euglossini"){

      #Eufriesea
      if (genus[i]==genus_eug[1]){

        res[i] <- exp(1.56 + 0.45 * log(IT_mm[i]))

        # Euglossa
      } else if (genus[i]==genus_eug[2]){

        res[i] <- exp(1.47 + 0.87 * log(IT_mm[i]))

        # Eulaema
      } else if (genus[i]==genus_eug[3]){

        res[i] <- exp(2.21 + 0.45 * log(IT_mm[i]))

        # Exarete
      } else if (genus[i]==genus_eug[4]){

        res[i] <- exp(2.33 + 0.45 * log(IT_mm[i]))

        # Euglossini general
      } else if (!(genus[i] %in% genus_eug)){

      res[i] <- exp(1.47 + 0.87 * log(IT_mm[i]))

      }

    } else {

      res[i] <- BeeIT::ITtongue(IT=IT_mm[i], family=fam[i], mouthpart="tongue")

    }

  }

  return(res)
}












