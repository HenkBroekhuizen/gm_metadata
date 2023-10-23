data.path <- "T:/Projecten/Project E-MOVO/2021 - Corona/3 Uitvoering/6 Data/GMJ 2021 analysebestand 7.0.sav"
metadata.excel.path <- "T:/GGDTwente/GGD-STAF/EGB/EPI/R/metadata/metadata_handmatig_2021.xlsx"

GenerateMetadata(
  data.path = data.path,
  id.prefix = "GMJE2021",
  metadata.excel.path = metadata.excel.path,
  target.path = dirname(metadata.excel.path)
)
md=read_json("GMJE2021_metadata")[[1]] %>% fromJSON %>% rrapply(how = "melt")

# DIT WERKT NOG NIET MET MEERDERE CONDITIONS
d <- LoadData(data.path, "GMJ2021_metadata", "topic==Leefstijl;data_type==double") %>% 
  ggd_SvyStepwiseRegression(outcome = "LACHGASS1")