sources <- c("Notícias das agências")

agencias <- c("ANA", "Anac", "Anatel", "Ancine", "Aneel", "ANM", 
              "ANP", "ANPD", "ANS", "Antaq", "ANTT", "Anvisa", "CVM")

options <- ""

DT_text_translations <- list(
    decimal = ",",
    emptyTable = "Sem dados disponíveis na tabela",
    info = "Exibindo _START_ a _END_ de _TOTAL_ entradas",
    infoEmpty = "Exibindo 0 a 0 de 0 entradas",
    infoFiltered = "(resultados filtrados de um total de _MAX_ entradas)",
    infoPostFix = "",
    thousands = ".",
    lengthMenu = "Exibir _MENU_ entradas",
    loadingRecords = "Carregando...",
    processing = "Processando...",
    search = "Buscar:",
    zeroRecords = "Não foram encontrados registros correspondentes",
    paginate = list(
      first = "Primeira",
      last =  "Última",
      `next` =  "Próxima",
      previous = "Anterior"
    ),
    aria = list(
      sortAscending = ": ativar ordenação ascendente da coluna",
      sortDescending = ": ativar ordenação descendente da coluna"
    )
)

inform_text <- "<p style = 'margin-bottom: 10px; line-height: 150%;'><i>Selecione o ID da notícia desejada (apenas o número) e clique no botão 'Ler notícia selecionada' para visualizar seu texto abaixo.</i></p>"
inform_table <- "<p style = 'margin-bottom: 10px; line-height: 150%;'><i>Clique em uma linha da tabela abaixo para selecionar o ID da notícia que deseja ler. Em seguida, clique no botão 'Ler notícia selecionada' para visualizar seu texto na aba ao lado.</i></p>"


restrict_options_from_source <- function(source) {
  if (source == sources[[1]]) {
    return(c("Todas as agências", agencias))
  }
}

describe_ref <- function(first_date, last_update) {
  list(
    first_date = paste0("<p><b>Monitoramento desde</b>: ", first_date, "</p>"),
    last_update = paste0("<p><b>Última atualização em</b>: ", last_update, "</p>")
  )
}

table_selection <- function(table, input_source, input_filter, input_date) {
  if (input_source == sources[1]) {
    table_selection_agencias(table, input_filter, input_date)
  }
}

br_date <- function(date) {
  dd <- as.character(date) %>% str_extract("\\d{2}$")
  mm <- as.character(date) %>% str_extract("(?<=-)\\d{2}")
  aaaa <- as.character(date) %>% str_extract("\\d{4}")
  return(paste(dd, mm, aaaa, sep = "/"))
}

prepare_text <- function(text, title, url, agencia, date) {
  
  wrap_paragraph <- function(text) {
    return(
      paste0("<p style = 'margin-bottom: 10px; line-height: 150%;'>", text, "</p>")
    )
  }
  
  paragraphs <- text %>%
    str_split("[\\n\\r]+") %>% 
    flatten() %>%
    map_chr(wrap_paragraph)
  
  div <- paste(
    "<div style = 'font: Verdana;'>", paragraphs, "</div>", collapse = "</br></br>"
  )
  
  agencia_ref <- paste0(
    "<p style = 'color: #183d7a; font-size: 120%; line-height: 150%;'><b>",
    agencia, "</b></p>"
  )
  
  date_ref <- paste0(
    "<p style = 'color: darkgray; line-height: 400%;'>",
    br_date(date), "</p>"
  )
  
  link <- paste0(
    "<p><i>Para acessar a notícia no site da agência, <a href= '",
    url, "'>clique aqui</a>.</i></p>"
  )
  
  return(paste0(h3(agencia, style = "color: #183d7a;"), h2(title), date_ref, link, hr(), "</br>", div))
}

table_selection_agencias <- function(tables, input_filter, input_date) {
  table <- tables[["agencias"]]
  cols <- c("id", "agencia", "date", "title", "description")
  new_column_names <- c("ID", "Agência", "Data de Publicação", "Título", "Chamada")
  output_table <- table %>% select(all_of(cols)) %>% filter(date >= input_date)
  names(output_table) <- new_column_names
  if (input_filter == "Todas as agências") {
    return(
      output_table
    )
  } else {
    return(
      output_table %>% filter(`Agência` == input_filter)
    )
  }
}

condense_references <- function(ids, agencias, datas) {
  condense_one_ref <- function(id, agencia, data) {
    paste(
      "<b>ID</b>:", id, "</br>", agencia, "</br>", data
    )
  }
  return(pmap_chr(list(ids, agencias, datas), condense_one_ref))
}

column_styles <- function(table) {
  table %>% 
    mutate(`Título` = paste0("<b>", `Título`, "</b>"),
           Chamada = paste0("<i>", `Chamada`, "</i>"),
           `Referências` = condense_references(ID, `Agência`, `Data de Publicação`)) %>% 
    select(all_of(c("Referências", "Título", "Chamada")))
}

