########################################################################################################################################
# Title: FEMC FHM species code translation function 
# Writer: Soren Donisvitch 
# Date 11/02/23
#
# Contact: soren.donisvitch@gmail.com
# Requirements: function requires tree species fhm codes in the forma of a dataframe

#' FEMC FHM species code translation function 
#'
#' @param data a dataframe of the tblTree and left joined tblTreeInfo from the vmc fhm and fhmma data tables
#' @param Species column name of species codes
#' @param return_name trigger to return new column as latin or common name. Default common name return, with required input of "Latin" 
#'
#' @return returns list translated species codes for dataframe
#' @export 
#'
#' @examples
#' # get latin name for dataframe as a new column
#' tblTree$LN = FHM_tree_species_code_translation(data=tblTree,Species = Species,"Latin_Name")
#' get common name for dataframe as a new column
#' tblTree$CN = FHM_tree_species_code_translation(data=tblTree,Species = Species)
########################################################################################################################################
FHM_tree_species_code_translation = function(data, Species, return_name = "Common") {
  LN = c()
  CN = c()
  WT = c()  # Adding a vector for Wood_Type
  
  for(i in 1:nrow(data)) {
    Species_i = data$Species[i]
    if(is.na(Species_i)){Latin_Name = NA; Common_Name = 'Unknown live tree sp.'; Wood_Type = NA}
      else if (Species_i == 10) {Latin_Name = 'Abies sp.'; Common_Name = 'Spruce'; Wood_Type = 'Softwood'}
      else if (Species_i == 317) {Latin_Name = 'Acer saccharinum'; Common_Name = 'Silver maple'; Wood_Type = 'Hardwood'}
      else if (Species_i == 94) {Latin_Name = 'Picea glauca'; Common_Name = 'White spruce'; Wood_Type = 'Softwood'}
      else if (Species_i == 543) {Latin_Name = 'Fraxinus nigra'; Common_Name = 'Black ash'; Wood_Type = 'Hardwood'}
      else if (Species_i == 541) {Latin_Name = 'Fraxinus americana'; Common_Name = 'White ash'; Wood_Type = 'Hardwood'}
      else if (Species_i == 544) {Latin_Name = 'Fraxinus pensylvanica'; Common_Name = 'Green ash'; Wood_Type = 'Hardwood'}
      else if (Species_i == 740) {Latin_Name = 'Populus sp.'; Common_Name = 'Aspen, poplar'; Wood_Type = 'Hardwood'}
      else if (Species_i == 743) {Latin_Name = 'Populus grandidentata'; Common_Name = 'Bigtooth aspen'; Wood_Type = 'Hardwood'}
      else if (Species_i == 746) {Latin_Name = 'Populus tremuloides'; Common_Name = 'Quaking aspen'; Wood_Type = 'Hardwood'}
      else if (Species_i == 742) {Latin_Name = 'Populus deltoides'; Common_Name = 'Eastern cottonwood'; Wood_Type = 'Hardwood'}
      else if (Species_i == 951) {Latin_Name = 'Tilia americana'; Common_Name = 'American Basswood'; Wood_Type = 'Hardwood'}
      else if (Species_i == 379) {Latin_Name = 'Betula populifolia'; Common_Name = 'Gray birch'; Wood_Type = 'Hardwood'}
      else if (Species_i == 372) {Latin_Name = 'Betula lenta'; Common_Name = 'Black birch; Sweet birch'; Wood_Type = 'Hardwood'}
      else if (Species_i == 901) {Latin_Name = 'Robinia pseudoacacia'; Common_Name = 'Black locust'; Wood_Type = 'Hardwood'}
      else if (Species_i == 313) {Latin_Name = 'Acer negundo'; Common_Name = 'Boxelder; Ash leaved maple'; Wood_Type = 'Hardwood'}
      else if (Species_i == 601) {Latin_Name = 'Juglans cinerea'; Common_Name = 'Butternut'; Wood_Type = 'Hardwood'}
      else if (Species_i == 972) {Latin_Name = 'Ulmus americana'; Common_Name = 'American elm'; Wood_Type = 'Hardwood'}
      else if (Species_i == 975) {Latin_Name = 'Ulmus rubra'; Common_Name = 'Slippery elm'; Wood_Type = 'Hardwood'}
      else if (Species_i == 400) {Latin_Name = 'Carya sp.'; Common_Name = 'Hickory sp.'; Wood_Type = 'Hardwood'}
      else if (Species_i == 701) {Latin_Name = 'Ostrya virginiana'; Common_Name = 'Hop hornbeam; Ironwood'; Wood_Type = 'Hardwood'}
      else if (Species_i == 837) {Latin_Name = 'Quercus velutina'; Common_Name = 'Black oak'; Wood_Type = 'Hardwood'}
      else if (Species_i == 832) {Latin_Name = 'Quercus prinus'; Common_Name = 'Chestnut oak'; Wood_Type = 'Hardwood'}
      else if (Species_i == 826) {Latin_Name = 'Quercus prinoides'; Common_Name = 'Chinquapin oak'; Wood_Type = 'Hardwood'}
      else if (Species_i == 833) {Latin_Name = 'Quercus rubra'; Common_Name = 'Northern red oak'; Wood_Type = 'Hardwood'}
      else if (Species_i == 804) {Latin_Name = 'Quercus bicolor'; Common_Name = 'Swamp white oak'; Wood_Type = 'Hardwood'}
      else if (Species_i == 802) {Latin_Name = 'Quercus alba'; Common_Name = 'White oak'; Wood_Type = 'Hardwood'}
      else if (Species_i == 806) {Latin_Name = 'Quercus coccinea'; Common_Name = 'Scarlet oak'; Wood_Type = 'Hardwood'}
      else if (Species_i == 356) {Latin_Name = 'Amelanchier sp.'; Common_Name = 'Serviceberry'; Wood_Type = 'Hardwood'}
      else if (Species_i == 68) {Latin_Name = 'Juniperus virginiana'; Common_Name = 'Eastern red cedar'; Wood_Type = 'Softwood'}
      else if (Species_i == 241) {Latin_Name = 'Thuja occidentalis'; Common_Name = 'Northern white cedar'; Wood_Type = 'Softwood'}
      else if (Species_i == 105) {Latin_Name = 'Pinus banksiana'; Common_Name = 'Jack pine'; Wood_Type = 'Softwood'}
      else if (Species_i == 126) {Latin_Name = 'Pinus rigida'; Common_Name = 'Pitch pine'; Wood_Type = 'Softwood'}
      else if (Species_i == 125) {Latin_Name = 'Pinus resinosa'; Common_Name = 'Red pine'; Wood_Type = 'Softwood'}
      else if (Species_i == 130) {Latin_Name = 'Pinus sylvestris'; Common_Name = 'Scotch pine'; Wood_Type = 'Softwood'}
      else if (Species_i == 129) {Latin_Name = 'Pinus strobus'; Common_Name = 'Eastern white pine'; Wood_Type = 'Softwood'}
      else if (Species_i == 91) {Latin_Name = 'Picea abies'; Common_Name = 'Norway spruce'; Wood_Type = 'Softwood'}
      else if (Species_i == 90) {Latin_Name = 'Picea sp.'; Common_Name = 'Spruce sp.'; Wood_Type = 'Softwood'}
      else if (Species_i == 71) {Latin_Name = 'Larix laricina'; Common_Name = 'Tamarack'; Wood_Type = 'Softwood'}
      else if (Species_i == 12) {Latin_Name = 'Abies balsamea'; Common_Name = 'Balsam fir'; Wood_Type = 'Softwood'}
      else if (Species_i == 315) {Latin_Name = 'Acer pensylvanicum'; Common_Name = 'Striped maple; Moose maple'; Wood_Type = 'Hardwood'}
      else if (Species_i == 316) {Latin_Name = 'Acer rubrum'; Common_Name = 'Red maple'; Wood_Type = 'Hardwood'}
      else if (Species_i == 318) {Latin_Name = 'Acer saccharum'; Common_Name = 'Sugar maple'; Wood_Type = 'Hardwood'}
      else if (Species_i == 319) {Latin_Name = 'Acer spicatum'; Common_Name = 'Mountain maple'; Wood_Type = 'Hardwood'}
      else if (Species_i == 371) {Latin_Name = 'Betula alleghaniensi'; Common_Name = 'Yellow birch'; Wood_Type = 'Hardwood'}
      else if (Species_i == 376) {Latin_Name = 'Betula papyrifera'; Common_Name = 'Paper or White birch'; Wood_Type = 'Hardwood'}
      else if (Species_i == 375) {Latin_Name = 'Betula papyrifera'; Common_Name = 'Paper or White birch'; Wood_Type = 'Hardwood'}
      else if (Species_i == 378) {Latin_Name = 'Betula papyrifera var cordifolia'; Common_Name = 'Mountain paper birch'; Wood_Type = 'Hardwood'}
      else if (Species_i == 531) {Latin_Name = 'Fagus grandifolia'; Common_Name = 'American beech'; Wood_Type = 'Hardwood'}
      else if (Species_i == 95) {Latin_Name = 'Picea mariana'; Common_Name = 'Black spruce'; Wood_Type = 'Softwood'}
      else if (Species_i == 97) {Latin_Name = 'Picea rubens'; Common_Name = 'Red spruce'; Wood_Type = 'Softwood'}
      else if (Species_i == 761) {Latin_Name = 'Prunus pennsylvanica'; Common_Name = 'Pin cherry'; Wood_Type = 'Hardwood'}
      else if (Species_i == 762) {Latin_Name = 'Prunus serotina'; Common_Name = 'Black cherry'; Wood_Type = 'Hardwood'}
      else if (Species_i == 920) {Latin_Name = 'Salix sp.'; Common_Name = 'Willow sp.'; Wood_Type = 'Hardwood'}
      else if (Species_i == 935) {Latin_Name = 'Sorbus americana'; Common_Name = 'American mountain ash'; Wood_Type = 'Hardwood'}
      else if (Species_i == 261) {Latin_Name = 'Tsuga canadensis'; Common_Name = 'Eastern hemlock'; Wood_Type = 'Softwood'}
      else if (Species_i == 800) {Latin_Name = 'Quercus sp.'; Common_Name = 'Oak sp.'; Wood_Type = 'Hardwood'}
      else if (Species_i == 310) {Latin_Name = 'Acer sp.'; Common_Name = 'Maple sp.'; Wood_Type = 'Hardwood'}
      else if (Species_i == 407) {Latin_Name = 'Carya ovata'; Common_Name = 'Shagbark hickory'; Wood_Type = 'Hardwood'}
      else if (Species_i == 693) {Latin_Name = 'Nyssa sylvatica'; Common_Name = 'Black gum'; Wood_Type = 'Hardwood'}
      else if (Species_i == 998) {Latin_Name = 'Decidua arbor'; Common_Name = 'Unknown deciduous spp.'; Wood_Type = 'Hardwood'}
      else if (Species_i == 540) {Latin_Name = 'Fraxinus'; Common_Name = 'Ash sp.'; Wood_Type = 'Hardwood'}
      else if (Species_i == 403) {Latin_Name = 'Carya glabra'; Common_Name = 'Pignut hickory'; Wood_Type = 'Hardwood'}
      else if (Species_i == 402) {Latin_Name = 'Carya cordiformis'; Common_Name = 'Bitternut hickory'; Wood_Type = 'Hardwood'}
      else if (Species_i == 409) {Latin_Name = 'Carya tomentosa'; Common_Name = 'Mockernut hickory'; Wood_Type = 'Hardwood'}
      else if (Species_i == 376) {Latin_Name = 'Betula papyrifera var. commutat'; Common_Name = 'Western paper birch'; Wood_Type = 'Hardwood'}
      else if (Species_i == 370) {Latin_Name = 'Betula sp.'; Common_Name = 'Birch sp.'; Wood_Type = 'Hardwood'}
      else if (Species_i == 378) {Latin_Name = 'Betula papyrifera var. commutat'; Common_Name = 'Northwestern paper birch'; Wood_Type = 'Hardwood'}
      else if (Species_i == 931) {Latin_Name = 'Sassafras papyrifera var. subcorda'; Common_Name = 'Sassafras'; Wood_Type = 'Hardwood'}
      else if (Species_i == 621) {Latin_Name = 'Liriodendron tulipifera'; Common_Name = 'Yellow poplar'; Wood_Type = 'Hardwood'}
      else if (Species_i == 421) {Latin_Name = 'Castanea dentata'; Common_Name = 'American chestnut'; Wood_Type = 'Hardwood'}
      else if (Species_i == 391) {Latin_Name = 'Carpinus caroliniana'; Common_Name = 'American hornbeam'; Wood_Type = 'Hardwood'}
      else if (Species_i == 585) {Latin_Name = 'Hamamelis virginiana'; Common_Name = 'Witch Hazel'; Wood_Type = 'Hardwood'}
      else if (Species_i == 357) {Latin_Name = 'Alnus sp.'; Common_Name = 'Alder sp.'; Wood_Type = 'Hardwood'}
      else if (Species_i == 10) {Latin_Name = 'Abies sp.'; Common_Name = 'Spruce'; Wood_Type = 'Softwood'}
      else if (Species_i == 0) {Latin_Name = 'Unknown species'; Common_Name = 'Unknown live tree sp.'; Wood_Type = 'Unknown'} 
      else if (Species_i == 57) {Latin_Name = 'Juniperus sp.'; Common_Name = 'Red cedar/juniper'; Wood_Type = 'Softwood'} 
      else if (Species_i == 314) {Latin_Name = 'Acer nigrum'; Common_Name = 'Black maple'; Wood_Type = 'Hardwood'} 
      else if (Species_i == 350) {Latin_Name = 'Alnus sp.'; Common_Name = 'Alder spp.'; Wood_Type = 'Hardwood'} 
      else if (Species_i == 351) {Latin_Name = 'Alnus rubra'; Common_Name = 'Red alder'; Wood_Type = 'Hardwood'} 
      else if (Species_i == 491) {Latin_Name = 'Cornus florida'; Common_Name = 'Flowering dogwood'; Wood_Type = 'Hardwood'}
      else if (Species_i == 591) {Latin_Name = 'Ilex opaca'; Common_Name = 'American holly'; Wood_Type = 'Hardwood'} 
      else if (Species_i == 760) {Latin_Name = 'Prunus sp.'; Common_Name = 'Cherry/plum sp.'; Wood_Type = 'Hardwood'} 
      else if (Species_i == 763) {Latin_Name = 'Prunus virginiana'; Common_Name = 'Chokecherry'; Wood_Type = 'Hardwood'} 
      else if (Species_i == 823) {Latin_Name = 'Quercus macrocarpa'; Common_Name = 'Bur oak'; Wood_Type = 'Hardwood'} 
      else if (Species_i == 602) {Latin_Name = 'Juglans nigra'; Common_Name = 'Black Walnut'; Wood_Type = 'Hardwood'}
      else if (Species_i == 816) {Latin_Name = 'Quercus ilicifolia'; Common_Name = 'Bear oak'; Wood_Type = 'Hardwood'} 
      else if (Species_i == 830) {Latin_Name = 'Quercus palustris'; Common_Name = 'Pin oak'; Wood_Type = 'Hardwood'} 
      else if (Species_i == 741) {Latin_Name = 'Populus balsamifera'; Common_Name = 'Balsam poplar'; Wood_Type = 'Hardwood'} 
      else if (Species_i == 221) {Latin_Name = 'Taxodium distichum'; Common_Name = 'Bald cypress'; Wood_Type = 'Hardwood'} 
      else if (Species_i == 999) {Latin_Name = NA; Common_Name = 'Unknown live tree sp.'; Wood_Type = NA}
      else if (Species_i == 998) {Latin_Name = NA; Common_Name = 'Unknown live tree sp.'; Wood_Type = NA}
      else if (Species_i == 299)  {Latin_Name = NA; Common_Name = 'Unknown dead softwood tree';  Wood_Type = 'Softwood'}
      else if (Species_i == 320)  {Latin_Name = 'Acer platanoides'; Common_Name = 'Norway maple'; Wood_Type = 'Hardwood'}
      else if (Species_i == 355)  {Latin_Name = 'Alnus glutinosa'; Common_Name = 'European alder';  Wood_Type = 'Hardwood'}
      else if (Species_i == 600)  {Latin_Name = 'Juglans sp.';  Common_Name = 'Walnut';  Wood_Type = 'Hardwood'}
      else if (Species_i == 660)  {Latin_Name = 'Malus sp.'; Common_Name = 'Apple';   Wood_Type = 'Hardwood'}
      else if (Species_i == 766)  {Latin_Name = 'Prunus americana'; Common_Name = 'Wild plum';   Wood_Type = 'Hardwood'}
      else if (Species_i == 835)  {Latin_Name = 'Quercus stellata'; Common_Name = 'Post oak'; Wood_Type = 'Hardwood'}
      else if (Species_i == 977)  {Latin_Name = 'Ulmus thomasii';  Common_Name = 'Rock elm'; Wood_Type = 'Hardwood'}
      else if (Species_i == 8421) {Latin_Name = 'Pyrus calleryana'; Common_Name = 'Callery pear'; Wood_Type = 'Hardwood'}
    else {Latin_Name = NA; Common_Name = 'Unknown live tree sp.'; Wood_Type = NA}
 
 LN[i] = Latin_Name
 CN[i] = Common_Name
 WT[i] = Wood_Type
  }
  
  if(return_name == "Latin") {
 return(LN)
  } else if (return_name == "Wood_Type") {
 return(WT)
  } else {
 return(CN)
  }
}
