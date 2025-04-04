library(shiny)
library(shinyWidgets)
library(DT)
library(shinythemes)
library(data.table)
library(readxl)

# Define UI
select_by_tissue <- tabPanel(
  title="Tissues",
  titlePanel("Select scRNAseq datasets based on brain region"),
  h3("About:"),
  h4("As an attempt to have an overview of in which tissues (and for the brain specifically, in which regions) scRNAseq data are available, I organized the regions based on the hierarchy in ", tags$a(href="https://atlas.brain-map.org/", "atlas.", target="_blank")),
  h3("Usage:"),
  h4("Sequentially select a level until the last level is reached where the datasets can be viewed."),
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "level_1",
        label = "Select Level 1:",
        choices = c("Brain", "Spinal Cord", "Blood", "Bone Marrow", "Heart", "Intestine", "Kidney", "Liver", "Lung", "Lymph Node", "Pancreas", "Skeletal Muscle", "Spleen"),
        selected = NULL,
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      uiOutput("level_2"),
      uiOutput("level_3"),
      uiOutput("level_4"),
      uiOutput("level_5"),
      uiOutput("level_6"),
      uiOutput("level_7"),
      uiOutput("level_8"),
      uiOutput("level_9"),
      uiOutput("last_level"),
      actionButton("clear", "Clear Selection"),
      downloadButton("download_data", "Download Selection")
    ),
    mainPanel(
      h3("You selected:", style="color:mediumblue"),
      textOutput("selected_level_1"),
      textOutput("selected_level_2"),
      textOutput("selected_level_3"),
      textOutput("selected_level_4"),
      textOutput("selected_level_5"),
      textOutput("selected_level_6"),
      textOutput("selected_level_7"),
      textOutput("selected_level_8"),
      textOutput("selected_level_9"),
      h3("Datasets selected", style="color:mediumblue"),
      verbatimTextOutput("selected_last_level"),
      
      # # New Tab Panel for displaying dataset tables
      # tabsetPanel(
      #   tabPanel("Selected Datasets", DTOutput("dataset_table"))
      # )
    )
  )
)

select_by_study <- tabPanel(
  title = "Studies",
  titlePanel("Select scRNAseq datasets based on studies"),
  h3("About:"),
  h4("Each scRNAseq study may contains cells from different regions and/or developmental stages. Here, I subset each scRNAseq study to dataset where each dataset is for each region at a developmental stage. For more details on how this was done for each study, check: ", tags$a(href="https://github.com/tanyaphung/scrnaseq_viewer/tree/main/notes", "github.", target="_blank")),
  br(),
  h3("Usage:"),
  h4("Select a study from the drop-down menu."),
  h4("Click on the Get information button."),
  br(),
  h3("Output:"),
  h4("Table ", tags$b("Information for selected study"), " gives you information about each study."),
  h4("Table ", tags$b("Datasets derived from this study"), " gives you a list of datasets that were derieved from each study."),
  h4("For each dataset from the Table ", tags$b("Datasets derived from this study"), ", you can click on it, which will output information about the cell type and number of cells per cell type."),
  h5("Note that if you select another study, the last table is not cleared by default and you will need to click on the second table to update it."),
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "study",
        label = "Select Study:",
        choices = c("1_Smith_2021", "2_Jakel_2019", "3_Leng_2021", "4_Aldinger_2021", "5_Bhaduri_2021", "6_Bakken_2021", "7_Kamath_2022", "8_Otero-Garcia_2022", "9_Ma_2022", "10_Gabitto_2023", "11_Seeker_2023", "12_Gittings_2023", "13_Jorstad_2023", "14_Siletti_2023", "15_Zhu_2023", "16_Jorstad_2023", "17_Velmeshev_2023", "18_Sepp_2023", "19_Phan_2024", "20_Nascimento", "21_TorresFlores", "22_Wang_2024", "23_Braun_2023", "24_Rexach_2024", "25_Dharshini_2024", "26_NM_2024", "27_Clarence_2025", "28_Tadross_2025", "29_Pan_2024", "30_Xu_2023", "31_Johansen_2023", "32_Travaglini_2020"),
        selected = NULL,
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      actionButton("submit", "Get information")
    ),
    mainPanel(
      h3("Information for selected study:"),
      tableOutput('table'),
      h3("Datasets derived from this study:"),
      DTOutput('ds_table'),
      h3("Cell type information for each dataset:"),
      DTOutput("selected_table")
      
      # New Tab Panel for displaying dataset tables
    )
  )
)

about_page <- tabPanel(
  title="About",
  titlePanel("About: "),
  h4("This is an Rshiny app aimed to select scRNAseq datasets that can be used in FUMA Cell type or just to generally view what kinds of tissues/regions that have scRNAseq data available."),
  h5(tags$b("Disclaimer:"), "there is no guarantee that the information is accurate and there might be errors/bugs. Use at own risk."),
  h5(tags$b("Citaton:"), "If you use this in your research, please consider citing the application and/or the github repo."),
  h5(tags$b("Developed by:"), "Tanya Phung"),
  h5(tags$b("Last update:"), " 2025-04-04"),
  h5(tags$b("References:"), " Besides the links to each study, I downloaded the scRNAseq data from", tags$a(href="https://cellxgene.cziscience.com/", "cellxgene.", target="_blank")),
  h4("Usage:"),
  h5("For each scRNAseq study (for example, from Siletti et al. 2023), if you want to know all the datasets that were generated from this study, go to the tab ", tags$b("Studies.")),
  h5("If you want to know all the datasets for a particular tissue/region of interest, go to the tab ", tags$b("Tissues."))
)

ui <- navbarPage(
  title = "scRNAseq viewer",
  theme = shinytheme('cerulean'),
  about_page,
  select_by_study,
  select_by_tissue
  
)

# Define Server
server <- function(input, output, session) {
  
  level1_level2 <- list(
  "Brain" = c("brain", "forebrain", "midbrain", "hindbrain"), 
  "Spinal Cord" = c("Spinal Cord"),
  "Blood" = c("Blood"),
  "Bone Marrow" = c("Bone Marrow"),
  "Heart" = c("Heart"),
  "Intestine" = c("Intestine"),
  "Kidney" = c("Kidney"),
  "Liver" = c("Liver"),
  "Lung" = c("Lung"),
  "Lymph Node" = c("Lymph Node"),
  "Pancreas" = c("Pancreas"),
  "Skeletal Muscle" = c("Skeletal Muscle"),
  "Spleen" = c("Spleen")
  )
  level2_level3 <- list(
    "forebrain" = c("forebrain",
                    "gray matter of forebrain",
                    "surface structures of forebrain",
                    "white matter of forebrain"),
    "midbrain" = c("midbrain"),
    "hindbrain" = c("hindbrain", "gray matter of the hindbrain")
    )
  level3_level4 <- list(
    "gray matter of forebrain" = c("transient structures of forebrain", "telencephalon", "diencephalon"),
    "surface structures of forebrain" = c("cerebral gyri and lobules"),
    "white matter of forebrain" = c("white matter"),
    "gray matter of the hindbrain" = c("metencephalon", "myelencephalon")
  )
  level4_level5 <- list(
    "telencephalon" = c("cerebral cortex", "cerebral nuclei", "hypothalamus", "thalamus"),
    "cerebral gyri and lobules" = c("middle temporal gyrus", "limbic lobe"),
    "metencephalon" = c("cerebellum", "pons", "tegmetum of medulla oblongata"),
    "myelencephalon" = c("medulla")
  )
  level5_level6 <- list(
    "cerebral cortex" = c("neocortex", "allocortex", "periallocortex"),
    "limbic lobe" = c("hippocampal gyrus (formation)"),
    "tegmetum of medulla oblongata" = c("efferent nuclei of cranial nerves in the medulla oblongata")
  )
  level6_level7 <- list(
    "neocortex" = c("frontal neocortex", "parietal neocortex", "temporal neocortex", "occipital neocortex", "cingulate neocortex", "insular neocortex"),
    "efferent nuclei of cranial nerves in the medulla oblongata" = c("vagal nucleus")
    )
  level7_level8 <- list(
    "frontal neocortex" = c("prefrontal cortex", "posterior frontal cortex", "orbital frontal cortex"),
    "parietal neocortex" = c("primary somatosensory cortex"),
    "temporal neocortex" = c("dorsolateral temporal neocortex"),
    "occipital neocortex" = c("primary visual cortex")
    )
  level8_level9 <- list(
    "prefrontal cortex" = c("dorsolateral prefrontal cortex", "ventrolateral prefrontal cortex"),
    "posterior frontal cortex" = c("primary motor cortex"),
    "dorsolateral temporal neocortex" = c("primary auditory cortex")
  )
  last_level <- list("brain" = c("443_Braun2023_Human_2023_FirstTrimester_Brain_CarnegieStage18",
                                "462_Braun2023_Human_2023_FirstTrimester_Head_CarnegieStage15"), 
                     "forebrain" = c("431_Wang2024_Human_2024_FirstTrimester_Forebrain", "452_Braun2023_Human_2023_FirstTrimester_Forebrain_9wpc", "453_Braun2023_Human_2023_FirstTrimester_Forebrain_10wpc", "454_Braun2023_Human_2023_FirstTrimester_Forebrain_11wpc", "455_Braun2023_Human_2023_FirstTrimester_Forebrain_CarnegieStage22", "456_Braun2023_Human_2023_FirstTrimester_Forebrain_CarnegieStage20", "457_Braun2023_Human_2023_FirstTrimester_Forebrain_13wpc", "458_Braun2023_Human_2023_FirstTrimester_Forebrain_12wpc", "459_Braun2023_Human_2023_FirstTrimester_Forebrain_14wpc", "460_Braun2023_Human_2023_FirstTrimester_Forebrain_CarnegieStage16", "461_Braun2023_Human_2023_FirstTrimester_Forebrain_CarnegieStage15"),
                     "midbrain" = c("472_Braun2023_Human_2023_FirstTrimester_Midbrain_9wpc", "473_Braun2023_Human_2023_FirstTrimester_Midbrain_10wpc", "474_Braun2023_Human_2023_FirstTrimester_Midbrain_CarnegieStage20", "475_Braun2023_Human_2023_FirstTrimester_Midbrain_13wpc", "476_Braun2023_Human_2023_FirstTrimester_Midbrain_12wpc", "477_Braun2023_Human_2023_FirstTrimester_Midbrain_15wpc", "478_Braun2023_Human_2023_FirstTrimester_Midbrain_CarnegieStage16", "479_Braun2023_Human_2023_FirstTrimester_Midbrain_CarnegieStage15", "78_Siletti_Midbrain.SN_Human_2022", "79_Siletti_Midbrain.SN-RN_Human_2022", "80_Siletti_Midbrain.PAG_Human_2022", "81_Siletti_Midbrain.IC_Human_2022", "82_Siletti_Midbrain.SC_Human_2022", "83_Siletti_Midbrain.PTR_Human_2022", "84_Siletti_Midbrain.PAG-DR_Human_2022", "85_Siletti_Midbrain.RN_Human_2022", "426_Kamath2022_Human_2022_SN"),
                     "hindbrain" = c("463_Braun2023_Human_2023_FirstTrimester_Hindbrain_CarnegieStage20", "464_Braun2023_Human_2023_FirstTrimester_Hindbrain_CarnegieStage15", "465_Braun2023_Human_2023_FirstTrimester_Hindbrain_13wpc", "466_Braun2023_Human_2023_FirstTrimester_Hindbrain_CarnegieStage16"),
                     "transient structures of forebrain" = c("247_Zhu2023_Neocortex_Human_2023_group1", "248_Zhu2023_Neocortex_Human_2023_group2", "256_Smith2021_MidgestationalNeocortex_Human_2021_MedialGanglionicEminence", "376_Velmeshev2023_PrePostNatal_Human_2023_group1_MGE", "257_Smith2021_MidgestationalNeocortex_Human_2021_CaudalGanglionicEminence", "371_Velmeshev2023_PrePostNatal_Human_2023_group1_CGE", "370_Velmeshev2023_PrePostNatal_Human_2023_group1_LGE", "350_Nascimento_GanglionicEminence_Human_Fetal_subpallial", "360_Nascimento_GanglionicEminence_Human_Fetal_interneuron", "377_Velmeshev2023_PrePostNatal_Human_2023_group1_GE"),
                     "telencephalon" = c("430_Wang2024_Human_2024_FirstTrimester_Telencephalon", "485_Braun2023_Human_2023_FirstTrimester_Telencephalon_9wpc", "486_Braun2023_Human_2023_FirstTrimester_Telencephalon_10wpc", "487_Braun2023_Human_2023_FirstTrimester_Telencephalon_11wpc", "488_Braun2023_Human_2023_FirstTrimester_Telencephalon_CarnegieStage20", "489_Braun2023_Human_2023_FirstTrimester_Telencephalon_13wpc", "490_Braun2023_Human_2023_FirstTrimester_Telencephalon_12wpc", "491_Braun2023_Human_2023_FirstTrimester_Telencephalon_14wpc", "492_Braun2023_Human_2023_FirstTrimester_Telencephalon_15wpc"),
                     "diencephalon" = c("449_Braun2023_Human_2023_FirstTrimester_Diencephalon_9wpc", "450_Braun2023_Human_2023_FirstTrimester_Diencephalon_CarnegieStage20", "451_Braun2023_Human_2023_FirstTrimester_Diencephalon_15wpc"),
                     "cerebral gyri and lobules" = c("387_Velmeshev2023_PrePostNatal_Human_2023_group2_STG", "253_Smith2021_MidgestationalNeocortex_Human_2021_ParietalLobe", "261_Smith2021_InfantNeocortex_Human_2021_TemporalLobe", "262_Smith2021_InfantNeocortex_Human_2021_ParietalLobe", "12_Siletti_CerebralCortex.STG_Human_2022", "31_Siletti_CerebralCortex.FuGt.TF_Human_2022", "35_Siletti_CerebralCortex.ITG_Human_2022", "325_Jorstad2023_AnG_Human_2023_10x", "11_Siletti_CerebralCortex.PPH.TH-TL_Human_2022", "425_Leng2021_Human_2021_SFG", "34_Siletti_CerebralCortex.SOG.A19_Human_2022"),
                     "middle temporal gyrus" = c("3_Gabitto_MTG_Human_2023", "226_Jorstadetal2023_MTG_Human_2023_smartseq", "227_Jorstadetal2023_MTG_Human_2023_10x", "324_Jorstad2023_MTG_Human_2023_10x", "331_Jorstad2023_MTG_Human_2023_Smartseq", "8_Siletti_CerebralCortex.MTG_Human_2022"),
                     "white matter" = c("5_Jakel_WhiteMatter_Human_2019", "332_Seeker2023_WhiteMatter_Human_2023_BA4_Young", "333_Seeker2023_WhiteMatter_Human_2023_BA4_Old", "334_Seeker2023_WhiteMatter_Human_2023_CB_Young", "335_Seeker2023_WhiteMatter_Human_2023_CB_Old", "336_Seeker2023_WhiteMatter_Human_2023_CSC_Young", "337_Seeker2023_WhiteMatter_Human_2023_CSC_Old"),
                     "myelencephalon" = c("86_Siletti_Myelencephalon.MoAN_Human_2022", "87_Siletti_Myelencephalon.MoRF-MoEN_Human_2022", "88_Siletti_Myelencephalon.PrCbN.IO_Human_2022", "89_Siletti_Myelencephalon.MoSR_Human_2022"),
                     "cerebral nuclei" = c("505_Clarence_Human_2025_CaudateNucleus_PostnatalEarly", "45_Siletti_CerebralNuclei.CEN_Human_2022", "48_Siletti_CerebralNuclei.CMN_Human_2022", "50_Siletti_CerebralNuclei.BLN.BL_Human_2022", "55_Siletti_CerebralNuclei.BLN.BM_Human_2022", "57_Siletti_CerebralNuclei.BLN.La_Human_2022", "58_Siletti_CerebralNuclei.GP.CMN.CoA_Human_2022", "51_Siletti_CerebralNuclei.BNST_Human_2022", "44_Siletti_CerebralNuclei.GP.Gpe_Human_2022", "46_Siletti_CerebralNuclei.SEP_Human_2022", "52_Siletti_CerebralNuclei.Pu_Human_2022", "53_Siletti_CerebralNuclei.GP.Gpi_Human_2022", "54_Siletti_CerebralNuclei.CaB_Human_2022", "56_Siletti_CerebralNuclei.NAC_Human_2022", "47_Siletti_CerebralNuclei.Cla_Human_2022", "49_Siletti_CerebralNuclei.SI_Human_2022", "427_Phan2024_Human_2024_CaudateNucleus", "428_Phan2024_Human_2024_Putamen", "504_NM2024_Human_2024_GlobusPallidus", "509_Clarence_Human_2025_CaudateNucleus_PostnatalLate"),
                     "hypothalamus" = c("70_Siletti_Hypothalamus.HTHma.MN_Human_2022", "71_Siletti_Hypothalamus.HTHpo.HTHso_Human_2022", "72_Siletti_Hypothalamus.HTHma.HTHtub_Human_2022", "73_Siletti_Hypothalamus.HTHso.HTHtub_Human_2022", "74_Siletti_Hypothalamus.HTHpo_Human_2022", "75_Siletti_Hypothalamus.HTHtub_Human_2022", "76_Siletti_Hypothalamus.HTHso_Human_2022", "77_Siletti_Hypothalamus.HTHma_Human_2022", "514_Tadross_Human_2025_Hypothalamus_Tadross", "515_Tadross_Human_2025_Hypothalamus_Siletti"),
                     "thalamus" = c("97_Siletti_Thalamus.PoN.LG_Human_2022", "98_Siletti_Thalamus.LNC.Pul_Human_2022", "99_Siletti_Thalamus.ANC_Human_2022", "100_Siletti_Thalamus.LNC.VLN_Human_2022", "101_Siletti_Thalamus.LNC.LP_Human_2022", "102_Siletti_Thalamus.ILN.PILN.CM.Pf_Human_2022", "103_Siletti_Thalamus.ETH_Human_2022", "104_Siletti_Thalamus.MNC.MD_Human_2022", "105_Siletti_Thalamus.STH_Human_2022", "106_Siletti_Thalamus.LNC.LP.VPL_Human_2022", "107_Siletti_Thalamus.PoN.MG_Human_2022", "108_Siletti_Thalamus.MNC.MD.Re_Human_2022", "109_Siletti_Thalamus.ILN.PILN.CM_Human_2022", "110_Siletti_Thalamus.LNC.VA_Human_2022", "111_Siletti_Thalamus.LNC.VPL_Human_2022"),
                     "cerebellum" = c("236_Sepp2023_Cerebellum_Human_2023_group1", "237_Sepp2023_Cerebellum_Human_2023_group2", "238_Sepp2023_Cerebellum_Human_2023_group3", "239_Sepp2023_Cerebellum_Human_2023_group4", "240_Sepp2023_Cerebellum_Human_2023_group5", "241_Sepp2023_Cerebellum_Human_2023_group6", "242_Sepp2023_Cerebellum_Human_2023_group7", "264_Aldinger2021_PrenatalCerebellum_Human_2021_9wpc", "265_Aldinger2021_PrenatalCerebellum_Human_2021_10wpc", "266_Aldinger2021_PrenatalCerebellum_Human_2021_11wpc", "267_Aldinger2021_PrenatalCerebellum_Human_2021_12wpc", "268_Aldinger2021_PrenatalCerebellum_Human_2021_14wpc", "269_Aldinger2021_PrenatalCerebellum_Human_2021_17wpc", "270_Aldinger2021_PrenatalCerebellum_Human_2021_18wpc", "271_Aldinger2021_PrenatalCerebellum_Human_2021_20wpc", "272_Aldinger2021_PrenatalCerebellum_Human_2021_21wpc", "444_Braun2023_Human_2023_FirstTrimester_Cerebellum_9wpc", "445_Braun2023_Human_2023_FirstTrimester_Cerebellum_CarnegieStage20", "446_Braun2023_Human_2023_FirstTrimester_Cerebellum_13wpc", "447_Braun2023_Human_2023_FirstTrimester_Cerebellum_12wpc", "448_Braun2023_Human_2023_FirstTrimester_Cerebellum_15wpc", "243_Sepp2023_Cerebellum_Human_2023_group8", "244_Sepp2023_Cerebellum_Human_2023_group9", "245_Sepp2023_Cerebellum_Human_2023_group10", "4_TorresFlores_Cerebellum_Human", "41_Siletti_Cerebellum.CBV_Human_2022", "42_Siletti_Cerebellum.CbDN_Human_2022", "43_Siletti_Cerebellum.CBL_Human_2022", "246_Sepp2023_Cerebellum_Human_2023_group11"),
                     "pons" = c("480_Braun2023_Human_2023_FirstTrimester_Pons_9wpc", "481_Braun2023_Human_2023_FirstTrimester_Pons_10wpc", "482_Braun2023_Human_2023_FirstTrimester_Pons_CarnegieStage20", "483_Braun2023_Human_2023_FirstTrimester_Pons_12wpc", "484_Braun2023_Human_2023_FirstTrimester_Pons_14wpc", "90_Siletti_Pons.PnRF_Human_2022", "91_Siletti_Pons.PnEN_Human_2022", "92_Siletti_Pons.PN_Human_2022", "93_Siletti_Pons.XPnTg.DTg_Human_2022", "94_Siletti_Pons.PnRF.PB_Human_2022", "95_Siletti_Pons.PnAN_Human_2022"),
                     "medulla" = c("467_Braun2023_Human_2023_FirstTrimester_Medulla_9wpc", "468_Braun2023_Human_2023_FirstTrimester_Medulla_10wpc", "469_Braun2023_Human_2023_FirstTrimester_Medulla_CarnegieStage20", "470_Braun2023_Human_2023_FirstTrimester_Medulla_14wpc", "471_Braun2023_Human_2023_FirstTrimester_Medulla_CarnegieStage18"),
                     "neocortex" = c("429_Wang2024_Human_2024_FirstTrimester_Neocortex", "434_Wang2024_Human_2024_SecondTrimester_Neocortex", "366_Velmeshev2023_PrePostNatal_Human_2023_group1_cortex", "538_Johansen_2023_Neocortex"),
                     "allocortex" = c("254_Smith2021_MidgestationalNeocortex_Human_2021_HippocampalFormation", "59_Siletti_Hippocampus.HiT.CA4-DGC_Human_2022", "60_Siletti_Hippocampus.HiH.HiT.Sub_Human_2022", "61_Siletti_Hippocampus.HiH.CA1_Human_2022", "62_Siletti_Hippocampus.HiH.CA1-3_Human_2022", "63_Siletti_Hippocampus.HiH.CA1-CA3_Human_2022", "64_Siletti_Hippocampus.HiH.DG-CA4_Human_2022", "65_Siletti_Hippocampus.HiB-RostralCA1-CA3_Human_2022", "66_Siletti_Hippocampus.HiH.CA2-3_Human_2022", "67_Siletti_Hippocampus.HiB.RostralCA1-2_Human_2022", "68_Siletti_Hippocampus.HiB.RostralDG-CA4_Human_2022", "69_Siletti_Hippocampus.HiB.RostralCA3_Human_2022", "549_Xu_Human_2023_Hippocampus", "20_Siletti_CerebralCortex.Pir_Human_2022", "32_Siletti_CerebralCortex.AON_Human_2022"),
                     "periallocortex" = c("345_Nascimento_EntorhinalCortex_Human_Fetal_subpallial", "355_Nascimento_EntorhinalCortex_Human_Fetal_interneuron", "346_Nascimento_EntorhinalCortex_Human_Infant_subpallial", "347_Nascimento_EntorhinalCortex_Human_Toddler_subpallial", "356_Nascimento_EntorhinalCortex_Human_Infant_interneuron", "357_Nascimento_EntorhinalCortex_Human_Toddler_interneuron", "365_Nascimento_EntorhinalCortex_Human_Infant_ECstream", "348_Nascimento_EntorhinalCortex_Human_Teen_subpallial", "349_Nascimento_EntorhinalCortex_Human_Adult_subpallial", "358_Nascimento_EntorhinalCortex_Human_Teen_interneuron", "359_Nascimento_EntorhinalCortex_Human_Adult_interneuron", "424_Leng2021_Human_2021_EC", "9_Siletti_CerebralCortex.APH.MEC_Human_2022", "28_Siletti_CerebralCortex.AG.LEC_Human_2022", "38_Siletti_CerebralCortex.PRG.A35-A36_Human_2022", "39_Siletti_CerebralCortex.A35.A35r_Human_2022", "25_Siletti_CerebralCortex.CgGrs.A29-A30_Human_2022", "23_Siletti_CerebralCortex.FI_Human_2022"),
                     "hippocampal gyrus (formation)" = c("506_Clarence_Human_2025_HippocampalFormation_PostnatalEarly", "510_Clarence_Human_2025_HippocampalFormation_PostnatalLate"),
                     "frontal neocortex" = c("380_Velmeshev2023_PrePostNatal_Human_2023_group2_FC", "341_Gittings_FrontalCortex_Human_2023_Part1", "342_Gittings_FrontalCortex_Human_2023_Part2"),
                     "parietal neocortex" = c("291_Bhaduri2021_PrenatalNeocortex_Human_2021_16wpc_ParietalCortex", "292_Bhaduri2021_PrenatalNeocortex_Human_2021_17wpc_ParietalCortex", "293_Bhaduri2021_PrenatalNeocortex_Human_2021_18wpc_ParietalCortex", "294_Bhaduri2021_PrenatalNeocortex_Human_2021_19wpc_ParietalCortex", "295_Bhaduri2021_PrenatalNeocortex_Human_2021_20wpc_ParietalCortex", "296_Bhaduri2021_PrenatalNeocortex_Human_2021_22wpc_ParietalCortex", "297_Bhaduri2021_PrenatalNeocortex_Human_2021_25wpc_ParietalCortex", "378_Velmeshev2023_PrePostNatal_Human_2023_group1_Frontoparietalcortex", "389_Velmeshev2023_PrePostNatal_Human_2023_group2_Frontoparietalcortex", "10_Siletti_CerebralCortex.PaO.A43_Human_2022", "14_Siletti_CerebralCortex.SMG_Human_2022", "22_Siletti_CerebralCortex.SPL.A5-A7_Human_2022", "498_Dharshini2024_Human_2024_BrodmannArea7"),
                     "temporal neocortex" = c("367_Velmeshev2023_PrePostNatal_Human_2023_group1_BA22", "383_Velmeshev2023_PrePostNatal_Human_2023_group2_BA22", "306_Bhaduri2021_PrenatalNeocortex_Human_2021_16wpc_TemporalCortex", "307_Bhaduri2021_PrenatalNeocortex_Human_2021_18wpc_TemporalCortex", "308_Bhaduri2021_PrenatalNeocortex_Human_2021_19wpc_TemporalCortex", "309_Bhaduri2021_PrenatalNeocortex_Human_2021_20wpc_TemporalCortex", "310_Bhaduri2021_PrenatalNeocortex_Human_2021_25wpc_TemporalCortex", "373_Velmeshev2023_PrePostNatal_Human_2023_group1_temporal", "388_Velmeshev2023_PrePostNatal_Human_2023_group2_temporal", "393_Velmeshev2023_PrePostNatal_Human_2023_group3_BA22", "398_Velmeshev2023_PrePostNatal_Human_2023_group4_BA22", "403_Velmeshev2023_PrePostNatal_Human_2023_group5_BA22", "409_Velmeshev2023_PrePostNatal_Human_2023_group6_BA22", "415_Velmeshev2023_PrePostNatal_Human_2023_group7_BA22", "19_Siletti_CerebralCortex.TP.A38_Human_2022"),
                     "occipital neocortex" = c("29_Siletti_CerebralCortex.V2_Human_2022", "36_Siletti_CerebralCortex.Pro_Human_2022", "343_Gittings_OccipitalCortex_Human_2023_Part1", "344_Gittings_OccipitalCortex_Human_2023_Part2"),
                     "cingulate neocortex" = c("259_Smith2021_MidgestationalNeocortex_Human_2021_AnteriorCingulateCortex", "375_Velmeshev2023_PrePostNatal_Human_2023_group1_ACC", "384_Velmeshev2023_PrePostNatal_Human_2023_group2_BA24", "374_Velmeshev2023_PrePostNatal_Human_2023_group1_cingulate", "381_Velmeshev2023_PrePostNatal_Human_2023_group2_cingulate", "391_Velmeshev2023_PrePostNatal_Human_2023_group3_BA24", "395_Velmeshev2023_PrePostNatal_Human_2023_group4_BA24", "401_Velmeshev2023_PrePostNatal_Human_2023_group5_BA24", "405_Velmeshev2023_PrePostNatal_Human_2023_group6_BA24", "508_Clarence_Human_2025_AnteriorCingulateCortex_PostnatalEarly", "323_Jorstad2023_ACC_Human_2023_10x", "330_Jorstad2023_ACC_Human_2023_Smartseq", "30_Siletti_CerebralCortex.ACC_Human_2022", "411_Velmeshev2023_PrePostNatal_Human_2023_group7_BA24", "419_Velmeshev2023_PrePostNatal_Human_2023_group8_BA24", "18_Siletti_CerebralCortex.SCG.A25_Human_2022", "37_Siletti_CerebralCortex.RoG.A32_Human_2022", "21_Siletti_CerebralCortex.CgGC.A23_Human_2022", "512_Clarence_Human_2025_AnteriorCingulateCortex_PostnatalLate"),
                     "insular neocortex" = c("408_Velmeshev2023_PrePostNatal_Human_2023_group6_FIC", "392_Velmeshev2023_PrePostNatal_Human_2023_group3_INS", "396_Velmeshev2023_PrePostNatal_Human_2023_group4_INS", "402_Velmeshev2023_PrePostNatal_Human_2023_group5_INS", "13_Siletti_CerebralCortex.LIG.Idg_Human_2022", "15_Siletti_CerebralCortex.Ig_Human_2022", "495_Rexach2024_Human_2024_InsularCortex", "414_Velmeshev2023_PrePostNatal_Human_2023_group7_FIC"),
                     "vagal nucleus" = c("500_NM2024_Human_2024_VagalNucleus"),
                     "prefrontal cortex" = c("276_Bhaduri2021_PrenatalNeocortex_Human_2021_16wpc_PrefrontalCortex", "277_Bhaduri2021_PrenatalNeocortex_Human_2021_17wpc_PrefrontalCortex", "278_Bhaduri2021_PrenatalNeocortex_Human_2021_18wpc_PrefrontalCortex", "279_Bhaduri2021_PrenatalNeocortex_Human_2021_19wpc_PrefrontalCortex", "280_Bhaduri2021_PrenatalNeocortex_Human_2021_20wpc_PrefrontalCortex", "281_Bhaduri2021_PrenatalNeocortex_Human_2021_22wpc_PrefrontalCortex", "282_Bhaduri2021_PrenatalNeocortex_Human_2021_25wpc_PrefrontalCortex", "372_Velmeshev2023_PrePostNatal_Human_2023_group1_PFC", "386_Velmeshev2023_PrePostNatal_Human_2023_group2_PFC", "432_Wang2024_Human_2024_SecondTrimester_PrefrontalCortex", "435_Wang2024_Human_2024_ThirdTrimester_PrefrontalCortex", "436_Wang2024_Human_2024_ThirdTrimester_BrodmannArea10", "260_Smith2021_InfantNeocortex_Human_2021_PrefrontalCortex", "406_Velmeshev2023_PrePostNatal_Human_2023_group6_PFC", "438_Wang2024_Human_2024_Infancy_BrodmannArea10", "339_OteroGarcia_NFTs_Human_2022", "412_Velmeshev2023_PrePostNatal_Human_2023_group7_PFC", "416_Velmeshev2023_PrePostNatal_Human_2023_group7_BA10", "423_Velmeshev2023_PrePostNatal_Human_2023_group8_BA10", "502_NM2024_Human_2024_PrefrontalCortex", "513_Pan_Human_2024_PrefrontalCortex"),
                     "dorsolateral prefrontal cortex" = c("379_Velmeshev2023_PrePostNatal_Human_2023_group1_BA9-46", "368_Velmeshev2023_PrePostNatal_Human_2023_group1_BA9", "385_Velmeshev2023_PrePostNatal_Human_2023_group2_BA9", "249_Zhu2023_Neocortex_Human_2023_group3", "250_Zhu2023_Neocortex_Human_2023_group4", "251_Zhu2023_Neocortex_Human_2023_group5", "390_Velmeshev2023_PrePostNatal_Human_2023_group3_BA9", "397_Velmeshev2023_PrePostNatal_Human_2023_group4_BA9", "400_Velmeshev2023_PrePostNatal_Human_2023_group5_BA9", "407_Velmeshev2023_PrePostNatal_Human_2023_group6_BA9", "394_Velmeshev2023_PrePostNatal_Human_2023_group3_BA8", "399_Velmeshev2023_PrePostNatal_Human_2023_group4_BA8", "404_Velmeshev2023_PrePostNatal_Human_2023_group5_BA46", "410_Velmeshev2023_PrePostNatal_Human_2023_group6_BA46", "440_Wang2024_Human_2024_Infancy_BrodmannArea9", "442_Wang2024_Human_2024_Adolescence_BrodmannArea9", "507_Clarence_Human_2025_DorsolateralPrefrontalCortex_PostnatalEarly", "322_Jorstad2023_DFC_Human_2023_10x", "252_Zhu2023_Neocortex_Human_2023_group6", "338_Ma2022_dlPFC_Human_2023", "417_Velmeshev2023_PrePostNatal_Human_2023_group7_BA9", "420_Velmeshev2023_PrePostNatal_Human_2023_group8_BA9", "418_Velmeshev2023_PrePostNatal_Human_2023_group7_BA8", "422_Velmeshev2023_PrePostNatal_Human_2023_group8_BA8", "413_Velmeshev2023_PrePostNatal_Human_2023_group7_BA46", "33_Siletti_CerebralCortex.MFG.A46_Human_2022", "497_Dharshini2024_Human_2024_BrodmannArea9", "511_Clarence_Human_2025_DorsolateralPrefrontalCortex_PostnatalLate"),
                     "ventrolateral prefrontal cortex" = c("16_Siletti_CerebralCortex.IFG.A44-A45_Human_2022"),
                     "primary motor cortex" = c("283_Bhaduri2021_PrenatalNeocortex_Human_2021_14wpc_PrimaryMotorCortex", "284_Bhaduri2021_PrenatalNeocortex_Human_2021_16wpc_PrimaryMotorCortex", "285_Bhaduri2021_PrenatalNeocortex_Human_2021_17wpc_PrimaryMotorCortex", "286_Bhaduri2021_PrenatalNeocortex_Human_2021_18wpc_PrimaryMotorCortex", "287_Bhaduri2021_PrenatalNeocortex_Human_2021_19wpc_PrimaryMotorCortex", "288_Bhaduri2021_PrenatalNeocortex_Human_2021_20wpc_PrimaryMotorCortex", "289_Bhaduri2021_PrenatalNeocortex_Human_2021_22wpc_PrimaryMotorCortex", "290_Bhaduri2021_PrenatalNeocortex_Human_2021_25wpc_PrimaryMotorCortex", "318_Jorstad2023_M1_Human_2023_10x", "326_Jorstad2023_M1_Human_2023_Smartseq", "273_Bakken2021_AdultM1_Human_2021", "421_Velmeshev2023_PrePostNatal_Human_2023_group8_PrimaryMotorCortex", "7_Siletti_CerebralCortex.PrCG.M1C_Human_2022", "496_Rexach2024_Human_2024_BrodmannArea4", "503_NM2024_Human_2024_PrimaryMotorCortex"),
                     "orbital frontal cortex" = c("258_Smith2021_MidgestationalNeocortex_Human_2021_OrbitofrontalCortex", "369_Velmeshev2023_PrePostNatal_Human_2023_group1_BA13", "382_Velmeshev2023_PrePostNatal_Human_2023_group2_BA13", "24_Siletti_CerebralCortex.POrG.A13_Human_2022", "40_Siletti_CerebralCortex.ReG.A14_Human_2022"),
                     "primary somatosensory cortex" = c("298_Bhaduri2021_PrenatalNeocortex_Human_2021_14wpc_PrimarySomatosensoryCortex", "299_Bhaduri2021_PrenatalNeocortex_Human_2021_16wpc_PrimarySomatosensoryCortex", "300_Bhaduri2021_PrenatalNeocortex_Human_2021_17wpc_PrimarySomatosensoryCortex", "301_Bhaduri2021_PrenatalNeocortex_Human_2021_18wpc_PrimarySomatosensoryCortex", "302_Bhaduri2021_PrenatalNeocortex_Human_2021_19wpc_PrimarySomatosensoryCortex", "303_Bhaduri2021_PrenatalNeocortex_Human_2021_20wpc_PrimarySomatosensoryCortex", "304_Bhaduri2021_PrenatalNeocortex_Human_2021_22wpc_PrimarySomatosensoryCortex", "305_Bhaduri2021_PrenatalNeocortex_Human_2021_25wpc_PrimarySomatosensoryCortex", "319_Jorstad2023_S1_Human_2023_10x", "327_Jorstad2023_S1_Human_2023_Smartseq", "17_Siletti_CerebralCortex.PoCG.S1C_Human_2022"),
                     "primary auditory cortex" = c("320_Jorstad2023_A1_Human_2023_10x", "328_Jorstad2023_A1_Human_2023_Smartseq", "27_Siletti_CerebralCortex.TTG.A1C_Human_2022"),
                     "primary visual cortex" = c("255_Smith2021_MidgestationalNeocortex_Human_2021_PrimaryVisualCortex", "311_Bhaduri2021_PrenatalNeocortex_Human_2021_14wpc_PrimaryVisualCortex", "312_Bhaduri2021_PrenatalNeocortex_Human_2021_16wpc_PrimaryVisualCortex", "313_Bhaduri2021_PrenatalNeocortex_Human_2021_17wpc_PrimaryVisualCortex", "314_Bhaduri2021_PrenatalNeocortex_Human_2021_18wpc_PrimaryVisualCortex", "315_Bhaduri2021_PrenatalNeocortex_Human_2021_19wpc_PrimaryVisualCortex", "316_Bhaduri2021_PrenatalNeocortex_Human_2021_20wpc_PrimaryVisualCortex", "317_Bhaduri2021_PrenatalNeocortex_Human_2021_22wpc_PrimaryVisualCortex", "433_Wang2024_Human_2024_SecondTrimester_VisualCortex", "437_Wang2024_Human_2024_ThirdTrimester_BrodmannArea17", "263_Smith2021_InfantNeocortex_Human_2021_PrimaryVisualCortex", "439_Wang2024_Human_2024_Infancy_BrodmannArea17", "441_Wang2024_Human_2024_Adolescence_BrodmannArea17", "321_Jorstad2023_V1_Human_2023_10x", "329_Jorstad2023_V1_Human_2023_Smartseq", "26_Siletti_CerebralCortex.LiG.V1C_Human_2022", "494_Rexach2024_Human_2024_PrimaryVisualCortex", "499_Dharshini2024_Human_2024_BrodmannArea17", "501_NM2024_Human_2024_PrimaryVisualCortex"),
                     "Spinal Cord" = c("96_Siletti_Spinalcord.Spc_Human_2022"),
                     "Blood" = c("539_Travaglini_2020_Blood", "541_Xu_Human_2023_Blood"),
                     "Bone Marrow" = c("542_Xu_Human_2023_Bone_marrow"),
                     "Heart" = c("543_Xu_Human_2023_Heart_LeftCardiacAtrium", "544_Xu_Human_2023_Heart_HeartLeftVentricle", "545_Xu_Human_2023_Heart_RightCardiacAtrium", "546_Xu_Human_2023_Heart_HeartRightVentricle", "547_Xu_Human_2023_Heart_ApexOfHeart", "548_Xu_Human_2023_Heart_InterventricularSeptum"),
                     "Intestine" = c("550_Xu_Human_2023_Intestine_AscendingColon", "551_Xu_Human_2023_Intestine_Caecum", "552_Xu_Human_2023_Intestine_ColonicEpithelium", "553_Xu_Human_2023_Intestine_DescendingColon", "554_Xu_Human_2023_Intestine_Duodenum", "555_Xu_Human_2023_Intestine_Ileum", "556_Xu_Human_2023_Intestine_Jejunum", "557_Xu_Human_2023_Intestine_LargeIntestine", "558_Xu_Human_2023_Intestine_Rectum", "559_Xu_Human_2023_Intestine_SigmoidColon", "560_Xu_Human_2023_Intestine_SmallIntestine", "561_Xu_Human_2023_Intestine_TransverseColon", "562_Xu_Human_2023_Intestine_VermiformAppendix"),
                     "Kidney" = c("563_Xu_Human_2023_Kidney_CortexOfKidney", "564_Xu_Human_2023_Kidney_RenalMedulla", "565_Xu_Human_2023_Kidney_RenalPapilla", "566_Xu_Human_2023_Kidney_Kidney", "567_Xu_Human_2023_Kidney_RenalPelvis"),
                     "Liver" = c("568_Xu_Human_2023_Liver_CaudateLobeOfLiver", "569_Xu_Human_2023_Liver_Liver"),
                     "Lung" = c("540_Travaglini_2020_Lung", "570_Xu_Human_2023_Lung_Lung", "571_Xu_Human_2023_Lung_LowerLobeOfLeftLung", "572_Xu_Human_2023_Lung_Bronchus", "573_Xu_Human_2023_Lung_UpperLobeOfLeftLung", "574_Xu_Human_2023_Lung_LungParenchyma"),
                     "Lymph Node" = c("575_Xu_Human_2023_Lymph_node_BronchopulmonaryLymphNode", "576_Xu_Human_2023_Lymph_node_ThoracicLymphNode", "577_Xu_Human_2023_Lymph_node_MesentericLymphNode", "578_Xu_Human_2023_Lymph_node_LymphNode"),
                     "Pancreas" = c("579_Xu_Human_2023_Pancreas_IsletOfLangerhans", "580_Xu_Human_2023_Pancreas_Pancreas"),
                     "Skeletal Muscle" = c("581_Xu_Human_2023_Skeletal_muscle_SkeletalMuscleOrganVertebrate", "582_Xu_Human_2023_Skeletal_muscle_MuscleOfPelvicDiaphragm", "583_Xu_Human_2023_Skeletal_muscle_RectusAbdominisMuscle", "584_Xu_Human_2023_Skeletal_muscle_MuscleOfAbdomen", "585_Xu_Human_2023_Skeletal_muscle_MuscleTissue"),
                     "Spleen" = c("586_Xu_Human_2023_Spleen")
)
  
  get_last_selected <- reactive({
    if (!is.null(input$level_9)) return(input$level_9)
    if (!is.null(input$level_8)) return(input$level_8)
    if (!is.null(input$level_7)) return(input$level_7)
    if (!is.null(input$level_6)) return(input$level_6)
    if (!is.null(input$level_5)) return(input$level_5)
    if (!is.null(input$level_4)) return(input$level_4)
    if (!is.null(input$level_3)) return(input$level_3)
    if (!is.null(input$level_2)) return(input$level_2)
    return(NULL)
  })
  
  # Function to generate dropdowns
  create_picker <- function(input_id, label, choices) {
    pickerInput(inputId = input_id, label = label, choices = choices, selected = NULL, multiple = TRUE, options = list(`live-search` = TRUE, "max-options" = 1))
  }
  
  output$level_2 <- renderUI({ req(input$level_1); create_picker("level_2", "Select Level 2:", level1_level2[[input$level_1]]) })
  output$level_3 <- renderUI({ req(input$level_2); create_picker("level_3", "Select Level 3:", level2_level3[[input$level_2]]) })
  output$level_4 <- renderUI({ req(input$level_3); create_picker("level_4", "Select Level 4:", level3_level4[[input$level_3]]) })
  output$level_5 <- renderUI({ req(input$level_4); create_picker("level_5", "Select Level 5:", level4_level5[[input$level_4]]) })
  output$level_6 <- renderUI({ req(input$level_5); create_picker("level_6", "Select Level 6:", level5_level6[[input$level_5]]) })
  output$level_7 <- renderUI({ req(input$level_6); create_picker("level_7", "Select Level 7:", level6_level7[[input$level_6]]) })
  output$level_8 <- renderUI({ req(input$level_7); create_picker("level_8", "Select Level 8:", level7_level8[[input$level_7]]) })
  output$level_9 <- renderUI({ req(input$level_8); create_picker("level_9", "Select Level 9:", level8_level9[[input$level_8]]) })
  
  output$last_level <- renderUI({
    req(get_last_selected())
    pickerInput(
      inputId = "last_level",
      label = "Select Dataset:",
      choices = last_level[[get_last_selected()]],
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })
  
  # Display selected levels
  output$selected_level_1 <- renderText({ req(input$level_1); paste("Selected Level 1:", input$level_1) })
  output$selected_level_2 <- renderText({ req(input$level_2); paste("Selected Level 2:", input$level_2) })
  output$selected_level_3 <- renderText({ req(input$level_3); paste("Selected Level 3:", input$level_3) })
  output$selected_level_4 <- renderText({ req(input$level_4); paste("Selected Level 4:", input$level_4) })
  output$selected_level_5 <- renderText({ req(input$level_5); paste("Selected Level 5:", input$level_5) })
  output$selected_level_6 <- renderText({ req(input$level_6); paste("Selected Level 6:", input$level_6) })
  output$selected_level_7 <- renderText({ req(input$level_7); paste("Selected Level 7:", input$level_7) })
  output$selected_level_8 <- renderText({ req(input$level_8); paste("Selected Level 8:", input$level_8) })
  output$selected_level_9 <- renderText({ req(input$level_9); paste("Selected Level 9:", input$level_9) })
  output$selected_last_level <- renderText({ req(input$last_level); paste(paste(input$last_level, collapse = "\n")) })
  
  # # Reactive expression to generate dataset table for last selected
  # get_dataset_table <- reactive({
  #   req(input$last_level)
  #   datasets <- input$last_level
  #   # Create a data frame as a placeholder (you can replace this with actual dataset lookup)
  #   dataset_table <- data.frame(
  #     Dataset = datasets,
  #     Size = rep(100, length(datasets)),  # Just an example, replace with actual data
  #     Type = rep("Single-cell", length(datasets))  # Just an example, replace with actual data
  #   )
  #   return(dataset_table)
  # })
  # 
  # # Render the dataset table in the "Selected Datasets" tab
  # output$dataset_table <- renderDT({
  #   get_dataset_table()
  # })
  
  # Clear selections
  observeEvent(input$clear, {
    updatePickerInput(session, "level_1", selected = character(0))
    updatePickerInput(session, "level_2", selected = character(0))
    updatePickerInput(session, "level_3", selected = character(0))
    updatePickerInput(session, "level_4", selected = character(0))
    updatePickerInput(session, "level_5", selected = character(0))
    updatePickerInput(session, "level_6", selected = character(0))
    updatePickerInput(session, "level_7", selected = character(0))
    updatePickerInput(session, "level_8", selected = character(0))
    updatePickerInput(session, "level_9", selected = character(0))
    updatePickerInput(session, "last_level", selected = character(0))
  })
  
  output$download_data <- downloadHandler(
    filename = function() { "selected_datasets.txt" },
    content = function(file) {
      writeLines(input$last_level, file)
    }
  )
  
  # For tab Studies
  # get information for each study
  studies_df = read_excel("scrnaseq_studies_info.xlsx")
  
  filtered_data = eventReactive(input$submit, {
    subset(studies_df, Study == input$study)
  })
  
  output$table <- renderTable({
    t_data <- t(filtered_data())
    colnames(t_data) <- rownames(filtered_data())
    t_data
  }, rownames = TRUE, colnames = FALSE)
  
  # get the datasets derived for each study
  datasets_df = read_excel("scrnaseq_studies_datasets.xlsx")
  
  filtered_datasets = eventReactive(input$submit, {
    subset(datasets_df, Study == input$study)
  })
  
  output$ds_table <- renderDT({
    data <- filtered_datasets()
    data$Datasets <- paste0("<a href='javascript:void(0);' onclick='Shiny.setInputValue(\"selected_dataset\", \"", data$Datasets, "\", {priority: \"event\"})'>", data$Datasets, "</a>")
    datatable(data, escape = FALSE, rownames = FALSE, options = list(dom = 't'))  
  }, sanitize.text.function = function(x) x, rownames = FALSE)
  
  
  # get the cells per dataset 
  cell_data = read.csv("all_ds_table3.csv")
  
  cell_data_filtered = eventReactive(input$selected_dataset, {
    subset(cell_data, ds == input$selected_dataset)
  })
  
  output$selected_table <- renderDT({
    if (is.null(input$selected_dataset)) return()
    cell_data_filtered()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
