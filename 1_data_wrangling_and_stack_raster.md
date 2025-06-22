
#### 🌍 ─────────────────────────────────────────────── 🌍

#### 🌍 THERESA ROCCO PEREIRA BARBOSA

#### 🌍 Brasileira \| Geocientista \| Dados

#### 🌍 <imakemapas@outlook.com.br> \| +55 24 998417085

#### 🌍 2025-06-16

#### 🌍 ─────────────────────────────────────────────── 🌍

<br>

##### Descrição: Este script renomeia os metadados dos rasters, reprojeta e aplica máscara usando um shapefile de limite, extrai valores de rasters para pontos, e prepara os dados para análise posterior, incluindo a limpeza de dados

<br>

#### 1. Configuração Inicial ————————————————-

``` r
# Limpar ambiente
rm(list = ls(all.names = TRUE))

# Carregar bibliotecas necessárias
library(janitor)   # Para normalização de nomes
library(stringr)   # Para manipulação de strings
library(terra)     # Para manipulação de dados raster e vetoriais
library(dplyr)     # Para manipulação de dados
library(tidyterra) # Para manipulação de dados terra
```

#### 2. Função para Resumir e Normalizar Nomes de Arquivos ——————–

``` r
# Função para resumir e normalizar nomes
resumir_nome   <- function(nome) {
  # Remove a extensão do arquivo
  nome_sem_ext <- tools::file_path_sans_ext(nome)
  
  # Normaliza o nome (remove espaços, caracteres especiais, etc.)
  nome_limpo   <- janitor::make_clean_names(nome_sem_ext)
  
  # Abrevia palavras comuns para tornar os nomes mais curtos
  nome_abreviado <- nome_limpo |>
    str_replace_all("ferric", "frc")        |>
    str_replace_all("ferrous", "frs")       |>
    str_replace_all("curv_", "crv_")        |>
    str_replace_all("slope", "slp")         |>
    str_replace_all("index", "idx")         |>
    str_replace_all("surface", "sfc")       |>
    str_replace_all("terrain", "ter")       |>
    str_replace_all("vertical", "vrt")      |>
    str_replace_all("horizontal", "hzt")    |>
    str_replace_all("effective", "eff")     |>
    str_replace_all("convergence", "cnv")   |>
    str_replace_all("diurnal", "dnl")       |>
    str_replace_all("standardized", "std")  |>
    str_replace_all("normalized", "nrm")    |>
    str_replace_all("mass_balance", "mbal") |>
    str_replace_all("rugosity", "rug")      |>
    str_replace_all("valley", "vly")        |>
    str_replace_all("vector", "vec")        |>
    str_replace_all("gossan", "gss")        |>
    str_replace_all("goethite", "goe")      |>
    str_replace_all("laterite", "later")    |>
    str_replace_all("claymineral", "cly")   |>
    str_replace_all("ferroginous", "fgn")   |>
    str_replace_all("oxide", "ox")          |>
    str_replace_all("iron", "fe")           |>
    str_replace_all("silicate", "slt")      |>
    str_replace_all("detection", "det")     |>
    str_replace_all("real_surface", "rsfc") |>
    str_replace_all("mid_slope", "mslp")    |> 
    str_replace_all("twi", "twi")           |>
    str_replace_all("ndvi", "ndvi")         |>
    str_replace_all("savi", "savi")         |>
    str_replace_all("vv", "vv")             |>
    str_replace_all("vh", "vh")             |>
    str_replace_all("tpi", "tpi")           |>
    str_replace_all("tst", "tst")           |>
    str_replace_all("mrvbf", "mrvbf")       |>
    str_replace_all("mrrtf", "mrrtf")       |>
    str_replace_all("elevation", "elev")     |>
    str_replace_all("aspect", "asp")        |>
    str_replace_all("hill", "hl")           |>
    str_replace_all("ls_factor", "lsf")     |>
    str_replace_all("saga_wetness", "swt")  |>
    str_replace_all("zonal_alteration", "zna")
  
  # Encurta o nome para 8 caracteres (ignorando a extensão)
  nome_resumido <- str_sub(nome_abreviado, 1, 8)
  
  # Adiciona a extensão de volta ao nome
  nome_final    <- paste0(nome_resumido, ".tif")
  
  return(nome_final)
}
```

#### 3. Renomeação dos Arquivos Raster e Metadados —————————

``` r
# Define os caminhos das pastas
raster_dir <- "../raster"          # Pasta com os rasters originais
output_dir <- "../raster_renamed"  # Pasta para salvar os rasters com metadados renomeados

# Cria a pasta de saída se ela não existir
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Lista todos os arquivos .tif na pasta de rasters
raster_files    <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE)

# Loop para renomear metadados e salvar os arquivos
for (file in raster_files) {
  # Extrai o nome do arquivo (sem o caminho)
  nome_original <- basename(file)
  
  # Resumir e normalizar o nome
  novo_nome     <- resumir_nome(nome_original)
  
  # Define o caminho completo do novo nome
  novo_caminho  <- file.path(output_dir, novo_nome)
  
  # Carrega o raster
  r             <- terra::rast(file)
  
  # Renomeia as camadas no SpatRaster
  names(r)      <- tools::file_path_sans_ext(novo_nome)
  
  # Salva o raster com os metadados renomeados
  terra::writeRaster(r, novo_caminho, filetype = "GTiff", overwrite = TRUE)
  
  # Exibe uma mensagem de confirmação
  cat(nome_original, "->", novo_nome, "\n")
}
```

    ## aspect.tif -> asp.tif 
    ## ast_ferric.tif -> ast_frc.tif 
    ## B11.tif -> b11.tif 
    ## B12.tif -> b12.tif 
    ## B2.tif -> b2.tif 
    ## B3.tif -> b3.tif 
    ## B4.tif -> b4.tif 
    ## B5.tif -> b5.tif 
    ## B6.tif -> b6.tif 
    ## B7.tif -> b7.tif 
    ## B8.tif -> b8.tif 
    ## B8A.tif -> b8a.tif 
    ## clayminera.tif -> claymine.tif 
    ## convergenc.tif -> converge.tif 
    ## curv_cross.tif -> crv_cros.tif 
    ## curv_flow_.tif -> crv_flow.tif 
    ## curv_gener.tif -> crv_gene.tif 
    ## curv_longi.tif -> crv_long.tif 
    ## curv_maxim.tif -> crv_maxi.tif 
    ## curv_minim.tif -> crv_mini.tif 
    ## curv_plan.tif -> crv_plan.tif 
    ## curv_profi.tif -> crv_prof.tif 
    ## curv_tange.tif -> crv_tang.tif 
    ## curv_total.tif -> crv_tota.tif 
    ## deteccao_a.tif -> deteccao.tif 
    ## diurnal_an.tif -> dnl_an.tif 
    ## effective_air_flow_heights.tif -> eff_air_.tif 
    ## elevation.tif -> elev.tif 
    ## ferric_iro.tif -> frc_iro.tif 
    ## ferric_oxi.tif -> frc_oxi.tif 
    ## ferricIron.tif -> frc_fe.tif 
    ## FerricOxides.tif -> frc_oxs.tif 
    ## ferroginou.tif -> ferrogin.tif 
    ## ferrous_ir.tif -> frs_ir.tif 
    ## ferrous_si.tif -> frs_si.tif 
    ## ferrousia.tif -> frsia.tif 
    ## ferrousIron.tif -> frs_fe.tif 
    ## ferroussil.tif -> frssil.tif 
    ## geol.tif -> geol.tif 
    ## goethitaha.tif -> goethita.tif 
    ## gossan_ast.tif -> gss_ast.tif 
    ## hill.tif -> hl.tif 
    ## hill_idx.tif -> hl_idx.tif 
    ## indice_laterita_vv_vh.tif -> indice_l.tif 
    ## laterite.tif -> later.tif 
    ## ls_factor.tif -> lsf.tif 
    ## mass_balan.tif -> mass_bal.tif 
    ## mid_slope.tif -> mid_slp.tif 
    ## mid_slope_.tif -> mid_slp.tif 
    ## mrrtf.tif -> mrrtf.tif 
    ## mrvbf.tif -> mrvbf.tif 
    ## ndvi.tif -> ndvi.tif 
    ## normalized.tif -> nrm.tif 
    ## real_surfa.tif -> real_sur.tif 
    ## rugosity_index.tif -> rug_idx.tif 
    ## rvi_index.tif -> rvi_idx.tif 
    ## saga_wetne.tif -> saga_wet.tif 
    ## savi.tif -> savi.tif 
    ## slope_degr.tif -> slp_degr.tif 
    ## slope_heig.tif -> slp_heig.tif 
    ## slope_idx.tif -> slp_idx.tif 
    ## standardiz.tif -> standard.tif 
    ## surface_sp.tif -> sfc_sp.tif 
    ## terrain_ru.tif -> ter_ru.tif 
    ## terrain_su.tif -> ter_su.tif 
    ## tpi.tif -> tpi.tif 
    ## tst.tif -> tst.tif 
    ## twi.tif -> twi.tif 
    ## valley.tif -> vly.tif 
    ## valley_dep.tif -> vly_dep.tif 
    ## valley_idx.tif -> vly_idx.tif 
    ## vector_rug.tif -> vec_rug.tif 
    ## vertical_h.tif -> vrt_h.tif 
    ## vv.tif -> vv.tif 
    ## zonasalter.tif -> zonasalt.tif

#### 4. Processamento dos Rasters (Reprojetar e Aplicar Máscara) ————-

``` r
# Define os caminhos das pastas e arquivos
shapefile_path   <- "../data/limite_mapeamento.shp"  # Caminho do shapefile de limite
output_dir_stack <- "../raster_stack"                # Pasta para salvar os rasters processados

# Cria a pasta de saída se ela não existir
if (!dir.exists(output_dir_stack)) {
  dir.create(output_dir_stack)
}

# Carrega o shapefile do limite da área de interesse
shapefile <- terra::vect(shapefile_path)

# Converte o shapefile para o CRS de destino (EPSG:32619)
shapefile <- terra::project(shapefile, "EPSG:32619")

# Lista os arquivos .tif na pasta 'raster_renamed'
raster_files_renamed <- list.files(output_dir, pattern = "\\.tif$", full.names = TRUE)

# Identificar o raster de elevação (padrão de alinhamento da extensao)
raster_ele <- raster_files_renamed[grepl("elev\\.tif$", raster_files_renamed)]

# Carrega o raster de elevação como referência
ref_raster <- terra::rast(raster_ele)
ref_raster <- terra::project(ref_raster, "EPSG:32619")
ref_raster <- terra::mask(ref_raster, shapefile)  # Garante mesma extensão da área de interesse

# Loop através de cada arquivo raster
for (raster_file in raster_files_renamed) {
  # Carrega o raster
  r <- terra::rast(raster_file)
  
  # Reprojeta o raster para o CRS de destino
  r_projected <- terra::project(r, "EPSG:32619")
  
  # Aplica a máscara usando o shapefile do limite da área de interesse
  r_masked <- terra::mask(r_projected, shapefile)
  
  # Resample para ter mesma extensão, resolução e alinhamento do raster de elevação
  r_aligned <- terra::resample(r_masked, ref_raster, method = "bilinear")
  
  # Atribui o nome corretamente
  names(r_aligned) <- tools::file_path_sans_ext(basename(raster_file))
  
  # Define o nome do arquivo de saída
  output_file <- file.path(output_dir_stack, basename(raster_file))
  
  # Salva o raster resultante
  terra::writeRaster(r_aligned, output_file, filetype = "GTiff", overwrite = TRUE)
}
```

#### 5. Carregar e Preparar o Shapefile de Pontos —————————-

``` r
# Carrega o shapefile de pontos
vect <- terra::vect("../data/samples_6l_aa.shp")
vect <- terra::project(vect, "EPSG:32619")

# Limpa dados
vect <- vect |> 
  tidyterra::select(c(Subclasses, x, y, Lat, Long)) |> 
  tidyterra::mutate(
    Subclasses = dplyr::case_when(
      Subclasses == "Laterite" ~ "laterite",
      Subclasses == "Nb-bearing laterite" ~ "Nb-bearing laterite",
      Subclasses == "Marginal Lake" ~ "marginal lake",
      Subclasses == "Marginal lake" ~ "marginal lake", 
      Subclasses == "Talus" ~ "talus",
      TRUE ~ Subclasses)) |> 
  tidyterra::filter(!is.na(Subclasses))

unique(vect$Subclasses)
```

    ## [1] "talus"               "laterite"            "marginal lake"       "Nb-bearing laterite"

``` r
# Reprojeta o shapefile de pontos para o CRS de destino (EPSG:32619)
vect_reproj <- terra::project(vect, "EPSG:32619")

# # Extrai as coordenadas dos pontos reprojetados
# coords <- terra::geom(vect_reproj)[, c("x", "y")]
# vect_reproj$x <- coords[, "x"]
# vect_reproj$y <- coords[, "y"]

# Lista todos os arquivos .tif na pasta 'raster_stack'
raster_files_stack <- list.files(output_dir_stack, pattern = "\\.tif$", full.names = TRUE)

# Carrega todos os rasters em um único objeto SpatRaster
st <- terra::rast(raster_files_stack)

# Verifica os nomes das camadas
print(names(st))

# === Transforma a camada geológica em fator com níveis definidos ===
st[["geol"]] <- as.factor(st[["geol"]])
unique(st$geol)
```

    ##  [1] "asp"      "ast_frc"  "b11"      "b12"      "b2"       "b3"       "b4"       "b5"       "b6"       "b7"       "b8"       "b8a"     
    ## [13] "claymine" "converge" "crv_cros" "crv_flow" "crv_gene" "crv_long" "crv_maxi" "crv_mini" "crv_plan" "crv_prof" "crv_tang" "crv_tota"
    ## [25] "deteccao" "dnl_an"   "eff_air_" "elev"     "ferrogin" "frc_fe"   "frc_iro"  "frc_oxi"  "frc_oxs"  "frs_fe"   "frs_ir"   "frs_si"  
    ## [37] "frsia"    "frssil"   "geol"     "goethita" "gss_ast"  "hl"       "hl_idx"   "indice_l" "lat"      "later"    "lsf"      "mass_bal"
    ## [49] "mid_slp"  "mrrtf"    "mrvbf"    "ndvi"     "nrm"      "real_sur" "rug_idx"  "rvi_idx"  "saga_wet" "savi"     "sfc_sp"   "slp_degr"
    ## [61] "slp_heig" "slp_idx"  "standard" "ter_ru"   "ter_su"   "tpi"      "tst"      "twi"      "vec_rug"  "vly"      "vly_dep"  "vly_idx" 
    ## [73] "vrt_h"    "vv"       "zonasalt"

    geol
    <chr>
    1	  0			
    16	1			
    34	2			
    55	3			
    76	4

#### 6. Extrair Valores dos Rasters para os Pontos —————————

``` r
# Extrai os valores dos rasters para os pontos
extracted_values <- terra::extract(st, vect_reproj, bind = TRUE)

# Exibe os valores extraídos
print(head(extracted_values))
```

    ##   Subclasses      x     y    Lat     Long       asp  ast_frc    b11    b12     b2     b3     b4     b5     b6     b7     b8    b8a
    ## 1      talus 758370 31610 758370 10031600 298.06799 1.900059 0.1708 0.0732 0.0274 0.0492 0.0260 0.0852 0.2339 0.2902 0.2894 0.3218
    ## 2      talus 758370 31590 758370 10031600  25.81403 1.876637 0.1708 0.0732 0.0274 0.0492 0.0260 0.0852 0.2339 0.2902 0.2894 0.3218
    ## 3      talus 760370 29470 760370 10029500 318.47485 1.999278 0.1826 0.0790 0.0300 0.0524 0.0294 0.0900 0.2486 0.3229 0.3262 0.3588
    ## 4      talus 760370 29450 760370 10029400 323.53461 1.928899 0.1826 0.0790 0.0300 0.0524 0.0294 0.0900 0.2486 0.3229 0.3262 0.3588
    ## 5      talus 760370 29430 760370 10029400 328.65649 1.931598 0.1849 0.0793 0.0273 0.0479 0.0249 0.0838 0.2475 0.3091 0.3028 0.3465
    ## 6      talus 760370 29410 760370 10029400 331.85693 1.948619 0.1654 0.0715 0.0287 0.0486 0.0274 0.0799 0.2364 0.3010 0.3071 0.3357
    ##   claymine  converge      crv_cros     crv_flow      crv_gene      crv_long      crv_maxi      crv_mini      crv_plan      crv_prof
    ## 1 2.333333  2.367930 -0.0003106905 1.576192e-09  0.0001864624  0.0004971530  4.361697e-04 -0.0003429385 -0.0624349602  0.0003076464
    ## 2 2.333333 37.073704  0.0006197599 2.602074e-09  0.0004147339 -0.0002050260  5.044497e-04 -0.0002970828  0.1143129468 -0.0002508762
    ## 3 2.311392 -3.394717 -0.0002629301 1.669256e-08 -0.0010320473 -0.0007691172  5.628895e-06 -0.0005216525 -0.0008931244 -0.0005005014
    ## 4 2.311392 -1.779500 -0.0002749672 6.805687e-08 -0.0010026550 -0.0007276878  1.028385e-05 -0.0005116113 -0.0027620918 -0.0004311198
    ## 5 2.331652 -1.541157 -0.0002322156 9.070433e-08 -0.0008715058 -0.0006392901 -3.230449e-06 -0.0004325224 -0.0023914769 -0.0003569164
    ## 6 2.313287 -1.199433 -0.0001672456 9.269387e-08 -0.0007026672 -0.0005354217  1.951705e-06 -0.0003532853 -0.0014431181 -0.0002942853
    ##        crv_tang     crv_tota  deteccao        dnl_an  eff_air_      elev ferrogin    frc_fe   frc_iro   frc_oxi   frc_oxs   frs_fe
    ## 1 -2.144194e-04 1.763444e-07 0.1111586 -0.0003332175 351.03653 300.49316 6.540650 0.5284553 0.5284553 0.5901866 0.5901866 2.145245
    ## 2  4.582455e-04 3.253720e-07 0.1111586 -0.0040019788 348.54550 300.56744 6.540650 0.5284553 0.5284553 0.5901866 0.5901866 2.145245
    ## 3 -1.529960e-05 2.574241e-07 0.1182623 -0.0075023542  10.19790  60.19172 6.847328 0.5610687 0.5610687 0.5597793 0.5597793 2.024496
    ## 4 -6.977243e-05 2.138134e-07 0.1182623 -0.0130219245  10.40058  60.51702 6.847328 0.5610687 0.5610687 0.5597793 0.5597793 2.024496
    ## 5 -7.822105e-05 1.482700e-07 0.1006061 -0.0192941967  10.67717  61.00458 7.233821 0.5198330 0.5198330 0.6106341 0.6106341 2.185584
    ## 6 -5.633148e-05 9.755821e-08 0.1159052 -0.0247474704  11.01245  61.63502 6.907408 0.5637860 0.5637860 0.5385868 0.5385868 2.006546
    ##      frs_ir    frs_si    frsia    frssil geol goethita   gss_ast hl hl_idx indice_l      lat    later          lsf     mass_bal    mid_slp
    ## 1 0.6044367 0.4285714 2.145245 0.4285714    2 1.795620 0.6044367  0      0 2.010417 2.333333 2.333333 4.313368e-02  0.009239117 0.03488645
    ## 2 0.6180540 0.4285714 2.145245 0.4285714    2 1.795620 0.6180540  0      0 1.123288 2.333333 2.333333 1.886264e-02  0.020320848 0.03300448
    ## 3 0.5914572 0.4326397 2.024496 0.4326397    1 1.746667 0.5914572  0      0 1.886364 2.311392 2.311392 6.667608e-05 -0.049014244 0.97154546
    ## 4 0.6178206 0.4326397 2.024496 0.4326397    1 1.746667 0.6178206  0      0 4.009615 2.311392 2.311392 7.895276e-05 -0.047659174 0.97147775
    ## 5 0.6072348 0.4288805 2.185584 0.4288805    1 1.754579 0.6072348  0      0 2.902778 2.331652 2.331652 1.272760e-04 -0.041664898 0.97072816
    ## 6 0.5919710 0.4322854 2.006546 0.4322854    1 1.693380 0.5919710  0      0 1.976562 2.313287 2.313287 9.272128e-02 -0.033852786 0.96812248
    ##        mrrtf    mrvbf      ndvi        nrm real_sur   rug_idx   rvi_idx  saga_wet      savi sfc_sp  slp_degr  slp_heig slp_idx  standard
    ## 1 0.08441857 2.828116 0.8351300 0.51744324 400.0023 0.4974093 1.3287197  8.792714 0.5233545      1 0.1967704 54.015774       1 183.91025
    ## 2 1.68204546 2.818839 0.8351300 0.51650226 400.0032 0.8902439 1.8838710  8.630418 0.5233545     -2 0.2296818 53.721581       1 183.72128
    ## 3 0.13410211 3.890306 0.8346457 0.01422727 400.0587 0.5301205 1.3858268 10.252618 0.5562936      1 0.9815489  3.078963       1  58.91735
    ## 4 0.29090828 3.707632 0.8346457 0.01426113 400.1277 0.2494005 0.7984645 10.139023 0.5562936      0 1.4474866  2.933663       1  58.92204
    ## 5 0.32285795 2.442884 0.8480318 0.01463593 400.2141 0.3444976 1.0249110 10.020436 0.5535919      0 1.8743798  2.805820       1  58.92978
    ## 6 0.25703242 1.597616 0.8361734 0.01593876 400.3051 0.5059289 1.3438320  9.894900 0.5358012      0 2.2370837  2.797003       1  58.94257
    ##       ter_ru      ter_su        tpi        tst       twi      vec_rug vly   vly_dep vly_idx vrt_h  vv zonasalt
    ## 1 0.06372403 50.14860535  0.1297427 0.10462838 16.400091 1.572841e-05   0  50.37399       0    96 193 1.694379
    ## 2 0.05650558 45.83905029  0.1548812 0.06054883  7.703533 4.686250e-05   0  50.28877       0   146 164 1.694379
    ## 3 0.22210787  0.00000000 -0.5550027 0.00000000 12.028236 2.478078e-05   0 213.33388       0   176 332 1.786418
    ## 4 0.32394612  0.07874816 -0.5840701 0.00000000 12.890798 1.540506e-05   0 202.77669       0   104 417 1.786418
    ## 5 0.41652885  0.62998527 -0.5425684 0.00000000 13.008066 1.174885e-05   0 188.90193       0   144 418 1.637642
    ## 6 0.49556744  2.12620044 -0.4553486 0.00000000  9.507901 7.928785e-06   0 172.68742       0   128 253 1.856711

``` r
dfextract <- as.data.frame(extracted_values) |> 
  dplyr::mutate(id = row_number())
```

#### 7. Salva dados com valores dos Rasters para os Pontos ——————

``` r
# Salva os dados limpos no diretório para modelagem
save(dfextract , file = '../data/dados_extract.RData')
```
