# 0. Setup inicial ##############
## Carrega as bibliotecas
library(cli)
library(dplyr)
library(glue)
library(lubridate)
library(purrr)
library(stringr)
library(micropan)

## Define PATH para os arquivos
## O que é PATH? É o endereço/caminho para a pasta onde estão os arquivos
## que foram baixados via instaloder

path <- "C:/minha_pasta_com_os_arquivos_baixados_do_instaloader/"

path <- "./data/cienciassociais_pucrio/"

## Carrega lista de nomes dos txt
jsonFiles <- list.files(path, pattern = "UTC.json.xz", recursive = TRUE)

# 1. Leitura dos txt ##############
## Cria função para ler cada txt, compactar em apenas
## uma string e por numa tibble
leitor <- function(file, numFile) {
  
  ### Expressa progresso
  cli::cli_inform("Extraindo postagem {.strong #{numFile}}")
  
  ### Lê o json
  dados = glue::glue("{path}/{file}") |> 
    micropan::xzuncompress(temporary = TRUE,
                           overwrite = TRUE,
                           remove = FALSE) |> 
    jsonlite::read_json()
  
  ### Extrai o banco
  banco = stringr::str_split_i(file, "/", 1) 
  
  ### Extrai o nome do perfil
  username = stringr::str_split_i(file, "/", 2) 
  
  ### Extrai a data e a converte
  data = file |> 
    #stringr::str_split_i("/", 3) |> 
    stringr::str_remove("_UTC\\.json.xz") |> 
    lubridate::ymd_hms()
  
  ### Extrai diversas informações
  #### 1: Likes da postagem
  #### 2: Num. de comentários na postagem
  #### 3: Texto da postagem
  #### 4: Username de perfis taggeados em mídias
  #### 5: Nomes de perfis taggeados em mídias
  #### 6: Local registrado
  #### 7: É vídeo?
  #### 8: Num. de visualizações do vídeo
  #### 9: Coautores da postagem
  #### 10: Num. de seguidores
  #### 11: Num. de perfis que segue
  #### 12: Bio. da conta
  #### 13: Nome da conta
  info <- rep(NA, 9) |> as.list()
  for (i in seq_along(info)) {
    
    extracao = try(
      silent = TRUE,
      switch(
        i,
        `1` = dados[["node"]][["edge_media_preview_like"]][["count"]], 
        `2` = dados[["node"]][["edge_media_to_comment"]][["count"]],
        `3` = dados[["node"]][["edge_media_to_caption"]][["edges"]][[1]][["node"]][["text"]],
        `4` = dados[["node"]][["edge_media_to_tagged_user"]][["edges"]] |>
          purrr::map_chr(~.$node$user$username) |> 
          unique() |> 
          glue::glue_collapse(sep = "|"),
        `5` = dados[["node"]][["edge_media_to_tagged_user"]][["edges"]] |>
          purrr::map_chr(~.$node$user$full_name) |> 
          unique() |> 
          glue::glue_collapse(sep = "|"),
        `6` = dados[["node"]][["location"]][["name"]],
        `7` = dados[["node"]][["is_video"]],
        `8` = dados[["node"]][["video_view_count"]],
        `9` = dados[["node"]][["coauthor_producers"]],
        `10` = dados[["node"]][["owner"]][["edge_followed_by"]][["count"]],
        `11` = dados[["node"]][["owner"]][["edge_follow"]][["count"]],
        `12` = dados[["node"]][["owner"]][["biography"]],
        `13` = dados[["node"]][["owner"]][["full_name"]]
      )
    )
    
    if (inherits(extracao, "try-error") | purrr::is_empty(extracao)) {
      extracao = NA
    }
    
    info[i] = extracao
    
  }
  
  ### Gera a tibble
  dplyr::tibble(
    banco = banco,
    username = username,
    data = data,
    likes = info[1][[1]],
    comentarios = info[2][[1]],
    texto = info[3][[1]],
    taggeadoUsername = info[4][[1]],
    taggeadoNome = info[5][[1]],
    local = info[6][[1]],
    video = info[7][[1]],
    videoViews = info[8][[1]],
    coautores = info[9][[1]],
    numSeguidores = info[10][[1]],
    numSegue = info[11][[1]],
    bio = info[12][[1]],
    nome = info[13][[1]]
  )
  
}

## Aplica a função à lista de arquivos
postagens <- jsonFiles |> 
  purrr::imap(leitor) |> 
  purrr::list_rbind()

## Substitui certos caracteres
postagens <- postagens |> 
  dplyr::mutate(across(
    .cols = where(is.character),
    .fns = ~stringr::str_replace_all(
      .,
      c("\n" = " ", "\t" = " ", "\"" = "'")
    )
  ))

## Elimina espaços em branco excessivos
postagens <- postagens |> 
  dplyr::mutate(texto = stringr::str_squish(texto))

## Salva o banco de dados
saveRDS(postagens, "./resultados/cienciassociais_pucrio.RDS")
write.table(postagens, row.names = FALSE, './resultados/postagens.csv', sep = ";")

## Opcional: mandar os dados para o Google Sheets

#install.packages("googlesheets4")

### Autorizar o uso de sua conta google pelo R 
googlesheets4::gs4_auth()

### Criar uma planilha Google com os dados

### - Substitua "Sample R" pelo título que preferir 
### - (eu colocaria o nome do perfil coletados)

(ss <- googlesheets4::gs4_create("cienciassociais_pucrio", sheets = postagens))



















