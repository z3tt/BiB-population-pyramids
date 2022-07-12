#' BiB Pyramiden Theme
#' 
#' Ein ggplot2 Theme für Populationspyramiden des Bundesinstitut für 
#' Bevölkerungsforschung
#' 
#' Ein "visual theme" für die Nutzung in Kombination mit der Grafikbibliothek 
#' ggplot2. Das `theme_bib()` nutzt die BundesSans Schriftart und spezifische 
#' Einstellungen für die Erstellung von Populationspyramiden mit ggplot2.
#' Das Theme nutzt zwei Schrifarten, Iosevka Slab und BundesSans Web. Beide 
#' Schrifarten müssen auf dem System lokal installiert sein; erstere ist als 
#' open-source Projekt unter https://github.com/be5invis/Iosevka verfügbar 
#' Die Schriftart BundesSans ist jedoch nicht frei erhältlich und muss ggf.
#' gegen eine andere, auf dem System instalierte Schriftart ausgetauscht werden.
#' 
#' 
#' @param base_size Referenzschriftgrüße (in pts)
#' @param base_family Hauptschriftart für alle Textelemente
#' @param base_line_size Voreingestellte Weite für Linienelemente
#' @param base_rect_size Voreingestellte Weite für Boxelemente
#' 
#' @example  
#' library(ggplot2)
#' ggplot(mpg, aes(x = class)) + geom_bar() + 
#'   labs(title = "Titel", caption = "Fusszeile") + theme_bib()

theme_bib <- function(base_size = 7, base_family = "BundesSans Web", 
                      axis_family = "Iosevka Slab",
                      base_line_size = base_size/22, 
                      base_rect_size = base_size/22) 
{
  ggplot2::theme_bw(base_size = base_size, base_family = base_family, 
                    base_line_size = base_line_size, 
                    base_rect_size = base_rect_size) %+replace% 
    ggplot2::theme(
      panel.background = ggplot2::element_blank(), 
      panel.border     = ggplot2::element_blank(), 
      strip.background = ggplot2::element_blank(), 
      plot.background  = ggplot2::element_blank(), 
      plot.margin      = ggplot2::margin(0, 0, 0, 0),
      
      text          = ggplot2::element_text(family = base_family, color = "black"),
      axis.title.x  = ggtext::element_markdown(size = base_size * 1.1, margin = ggplot2::margin(t = 4, b = 0)),
      axis.title.y  = ggplot2::element_blank(),
      axis.text.y   = ggplot2::element_text(
                        family = "Iosevka Slab", size = base_size, color = "grey25", 
                        margin = ggplot2::margin(0, 2.5, 0, 0)
                      ),
      axis.text.x   = ggplot2::element_text(
                        family = "Iosevka Slab", size = base_size, color = "grey25", 
                        margin = ggplot2::margin(2.5, 0, 2.5, 0)
                      ), 
      
      panel.grid.major.x  = ggplot2::element_blank(), 
      panel.grid.minor.x  = ggplot2::element_blank(), 
      panel.grid.major.y  = ggplot2::element_line(color = "grey70", size = .25),
      panel.grid.minor.y  = ggplot2::element_line(color = "grey70", size = .125),
      axis.line           = ggplot2::element_line(size = .25, color = "grey70"),
      axis.line.y         = ggplot2::element_blank(),
      axis.ticks.x        = ggplot2::element_line(size = .25, color = "grey70"),
      axis.ticks.y        = ggplot2::element_line(size = .25, color = "grey70"),
      axis.ticks.length.x = grid::unit(.15, "cm"),
      axis.ticks.length.y = grid::unit(.1, "cm"),
      
      legend.position       = "bottom",
      legend.margin         = ggplot2::margin(4, 7, 4, 0),
      legend.key.width      = grid::unit(.7, 'cm'), 
      legend.key.height     = grid::unit(.25, 'cm'),
      legend.text           = ggplot2::element_text(size = base_size, color = "grey25"), 
      legend.background     = ggplot2::element_rect(colour = "grey95", fill = "grey95"),
      legend.box.background = ggplot2::element_rect(colour = "grey95", fill = "grey95"),
      
      complete = TRUE
    )
}


#' Einfügen von "subticks" ohne Labels
#' 
#' Fügt nur für jedes n-te Tick auf den Achsen ein Label hinzu
#' (Quelle: stackoverflow.com)
#'
#' @param x Vektor mit allen Ticks
#' @param nth Anzahl der übersrpungenen Ticks
#' @param inverse Wenn TRUE (default) wird nur jedes n-te Label hinzugefügt;
#'                wenn FALSE wird jedes n-te Label entfernt
#' 
#' @examples
#' every_nth(1:10, nth = 3) 
#' every_nth(1:10, nth = 3, inverse = FALSE) 

every_nth <- function(x, nth, inverse = TRUE, label = NULL) 
{
  if (!inverse) {
    ## use an empty or a specified label as replacement
    if (is.null(label)) label <- ""
    x[1:nth == 1] <- label
    x
  } else {
    t <- x
    x[1:nth != 1] <- ""
    ## add a label to max tick number
    x <- ifelse(x == as.character(max(t)), paste0(x, label), x)
    x
  }
}


#' Vorbereitung von BiB Daten für die draw_pyramid() Funktion
#' 
#' Einlesen und Aufbereiten der Daten. Der Pfad zu den Daten sowie der Name der
#' xlsx-Datei ist vorgegeben (da immer gleich). Die xsls-Datei liegt in unserem
#' Fall in einem Unterordner namens `data`. Die hier angegebenen Originalaten 
#' unterliegen einer Sperrfrist und sind daher nicht Teil des Repositories.
#' 
#' @param file Die xsls Datei in mit den Bevölkerungsdaten in einem vordefiniertem
#'             Format mit jedem Thema pro Jahr in einem eigenen Tabellenblatt
#' @param sheet Das einzulesende Arbeitsblatt, entweder numerisch oder der Name
#' @param range Die einzulesenden Spalten und Zeilen, bei uns immer gleich

prep_data_pyramid <- function(file = here::here("data", "`Daten_Pyramiden_Broschuere_Gesamt.xlsx`"), 
                              sheet, range = "A4:O999") {
  
  data <- 
    readxl::read_excel(file, sheet, range = range) %>% 
    dplyr::select(-starts_with("...")) %>% 
    dplyr::rename(Alter = "Alter in Jahren")
  
  header <- names(data)
  header <- stringr::str_replace_all(header, "_", " ")
  header <- stringr::str_replace_all(header, "ae", "ä")
  header <- stringr::str_replace_all(header, "oe", "ö")
  header <- stringr::str_replace_all(header, "ue", "ü")
  header <- stringr::str_replace_all(header, "Ae", "Ä")
  header <- stringr::str_replace_all(header, "Oe", "Ö")
  header <- stringr::str_replace_all(header, "Ue", "Ü")
  
  ## korrekt "Fraün"
  header <- stringr::str_replace_all(header, "Fraün", "Frauen")
  
  names(data) <- header
  
  return(data)
}



#' Erstellen einer Pyramidenhälfte
#' 
#' Ausarbeiten einer nach links oder rechts orientierten Hälfte der 
#' Bevölkerungspyramide. Die Daten müssen zuvor mit Hilfe der Funktion
#' `prep_data_pyramid()` eingelesen und aufbereitet sein.
#' 
#' @param data Der mit der Funktion `prep_data_pyramid()` vorbereitete Datensatz
#' @param side Soll die Pyramide nach links (Männer, `"left"`) oder rechts 
#'             (Frauen, `"right"`) orientiert sein?
#' @param colors Farbvektor für die einzelnen Gruppen
#' @param outline Soll eine desaturierte Umrandung gezeichnet werden?
#' @param axis_family Die Schriftart für den numerischen Achsentext. Die 
#'                    Schriftart muss lokal auf dem System installier sein.
#' @param title_x Titel für die x-Achse
#' @param limit_age Maximales Alter auf der y-Achse
#' @param steps_age Schrittlänge für die kleinen y-Achsenteilstriche
#' @param labs_nth_age Schrittlänge  für die großen y-Achsenteilstriche mit Labels
#' @param limit_count Der maximale Wert der x-Achse
#' @param steps_count Anzahl der Achsenteilstriche auf der x-Achse
#' @param max_label_count Der x-Achsenwert für das letzte, maximale Label
#' @param labs_nth_count Schrittlänge für die großen x-Achsenteilstriche mit Labels
#'                           
#' @return ggplot Objekt (Pyramidenhälfte)

draw_pyramid_side <- function(data, side = "right", colors, outline = TRUE, 
                              axis_family = "Iosevka Slab", title_x = NULL, 
                              limit_age = 90, steps_age = 1, labs_nth_age = 5, 
                              limit_count = NA, steps_count = limit_count / 4, 
                              max_label_count = NA, labs_nth_count = 1) {
  
  ## Extrahieren der verschiedenen Gruppen in dem gewünschten Datensatz
  groups <- rev(unique(substr(names(data), 3, nchar(names(data))))[-1])
  
  ## Parameter basierend für gewünschte Geschlechts
  if (side == "left")  {
    prefix <- "M "   ## zum Filtern der Spalten + Standardiesieren der Gruppenamen
    hjust_axis <- 1  ## Position des x-Achsentitels
  }
  
  if (side == "right") {
    prefix <- "F "
    hjust_axis <- 0
  }
  
  ## Levels inkl. Lücken zwischen Gruppen in der Legende, 
  ## ohne dass am Ende eine leerer Bereich entsteht -
  ## wird im nächsten Schritt genutzt, um einen Faktor zu erstellen
  levels <- ifelse(groups == groups[1], groups, paste0(groups, "        "))
  
  data_grp <- 
    data %>% 
    ## Auswählen von der Werte für das gewünschte Geschlecht
    dplyr::select(Alter, dplyr::starts_with(prefix)) %>%
    ## Erstellen einer neuen Gruppenvariable mit zugehöriger Personenzahl (x1000)
    tidyr::pivot_longer(cols = -c("Alter"), names_to = "group", values_to = "value") %>% 
    ## Enternen des Geschlechts von den Gruppennamen und füge Lücken hinzu (wie oben)
    dplyr::mutate(
      group = stringr::str_remove(group, prefix),
      group = ifelse(group == groups[1], group, paste0(group, "        ")),
      group = factor(group, levels = levels)
    ) %>% 
    group_by(Alter) %>% 
    mutate(value = cumsum(value)) %>% 
    ungroup()
  
  ## Base Graph (für beide Geschlechter gleich) --------------------------------
  
  p_base <- 
    ggplot2::ggplot(data_grp, ggplot2::aes(x = Alter, ymin = 0, ymax = value)) +
    
    ggplot2::annotate(
      geom = "rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf, 
      fill = "white", color = "white", size = .25
    ) +
    
    ggplot2::coord_flip(clip = "off") +
    
    ggplot2::scale_fill_manual(values = colors, name = NULL) +
    
    ggplot2::labs(x = NULL, y = title_x) + 
    
    theme_bib(axis_family = axis_family) +
    
    ggplot2::theme(
      axis.title.x        = ggtext::element_markdown(hjust = hjust_axis),
      axis.text.y         = ggplot2::element_blank()
    )
  
  ## Männliche Hälfte
  if (side == "left") {
    
    ## Außenlinie zu den Bändern hinzufügen?
    if (outline == TRUE) {
      
      p_geom <- 
        p_base +
        ## Bänder je Gruppe mit desaturierter Außenline
        ggplot2::geom_ribbon(
          aes(fill = group, color = after_scale(colorspace::desaturate(colorspace::lighten(fill, .25), .15))),
          position = "identity", size = .25, outline.type = "full", key_glyph = draw_key_rect
        ) +
        
        ## seitliche Linie zum Abschneiden des Overlaps
        ## Durch dier Transformation für links-orientierte Pyramidenhälften ist 
        ## die Außenlinie der äußersten Gruppe auch etwas unterhalb der Nulllinie
        ## sichtbar. Dieser "Fehler" wird hier überzeichnet; der Wert der weißen
        ##Linie wurde schrittweise so optimiert, dass alle tatsächlichen Daten
        ## sichtbar sind.
        ggplot2::geom_hline(
          yintercept = -3.5, color = "white", size = .225
        ) 
      
    } else {
      
      p_geom <- 
        p_base +
        ## Bänder ohne Außenlinie
        ggplot2::geom_area(
          aes(fill = group),
          position = "stack", outline.type = "full", key_glyph = draw_key_rect
        )
      
    }
    
    p <- 
      p_geom +
      
      ## Anpassung der x-Achse
      ggplot2::scale_x_continuous(
        expand = c(0, 0),
        breaks = seq(0, limit_age, by = labs_nth_age),
        minor_breaks = seq(0, limit_age, by = steps_age),
        limits = c(0, limit_age * 1.0015),
        position = "top"
      ) +
      
      ## Anpassung der y-Achse
      ggplot2::scale_y_continuous(
        expand = c(0, 0), 
        trans = "reverse",
        breaks = seq(0, max_label_count, by = steps_count),
        labels = function(y) every_nth(y, labs_nth_count, inverse = TRUE), 
        limits = c(limit_count, 0 - limit_count / 45)
      )
    
  }
  
  ## weibliche Hälfte
  if (side == "right") {
    
    ## Außenlinie zu den Bändern hinzufügen?
    if (outline == TRUE) {
      
      p_geom <- 
        p_base +
        ## Bänder je Gruppe mit desaturierter Außenline
        ggplot2::geom_ribbon(
          aes(fill = group, color = after_scale(colorspace::desaturate(colorspace::lighten(fill, .25), .15))), 
          position = "identity", size = .25, outline.type = "full", key_glyph = draw_key_rect
        ) +
        
        ## seitliche Linie zum Abschneiden des Overlaps
        ggplot2::geom_hline(
          yintercept = -3.5, color = "white", size = .255
        ) 
      
      
    } else {
      
      p_geom <- 
        p_base +
        ## Bänder ohne Außenlinie
        ggplot2::geom_area(
          aes(fill = group),
          position = "stack", outline.type = "full", key_glyph = draw_key_rect
        )
      
    }
    
    p <- 
      p_geom +
      
      ## Anpassung der x-Achse
      ggplot2::scale_x_continuous(
        expand = c(0, 0),
        breaks = seq(0, limit_age, by = labs_nth_age),
        minor_breaks = seq(0, limit_age, by = steps_age),
        limits = c(0, limit_age * 1.0015)
      ) +
      
      ## Anpassung der y-Achse
      ggplot2::scale_y_continuous(
        expand = c(0, 0), 
        breaks = seq(0, max_label_count, by = steps_count),
        labels = function(y) every_nth(y, labs_nth_count, inverse = TRUE),
        limits = c(0 - limit_count / 45, limit_count)
      )
    
  }
  
  ## Anpassung der Legende
  p <- p + guides(fill = guide_legend(reverse = TRUE, nrow = 1))
  
  return(p)
  
}



#' Erstellen einer Gesamtpyramide
#' 
#' Kombinieren der Pyramidenhälften zu einer vollständigen Bevölkerungspyramide
#' inklusive Achsenbeschriftung und Titel. Die Funktion ruft dieSubfunktion
#' `draw_pyramid_side()` auf und greift daher diese Argument wieder auf um diese
#' einheitlich zu setzen und an die Subfunktion weiterzugeben.
#' 
#' @param data Der mit der Funktion `prep_data_pyramid()` vorbereitete Datensatz
#' @param title Der Titel für die Pyramide
#' @param outline Soll eine desaturierte Umrandung gezeichnet werden?
#' @param colors Farbvektor für die einzelnen Gruppen
#' @param axis_family Die Schriftart für den numerischen Achsentext. Die 
#'                    Schriftart muss lokal auf dem System installier sein.
#' @param limit_age Maximales Alter auf der y-Achse
#' @param steps_age Schrittlänge für die kleinen y-Achsenteilstriche
#' @param labs_nth_age Schrittlänge  für die großen y-Achsenteilstriche mit Labels
#' @param label_age Text für die y-Achse; dieser wird enweder über oder unter
#'                  dem letzen Wert hinzugefügt je nachdem, ob der Text mit einem
#'                  Zeilenumbruch `\n` beginnt (in unserem Fall "\nJahre", um 
#'                  es unter der 90 zu platzieren)
#' @param limit_count Der maximale Wert der x-Achse
#' @param steps_count Anzahl der Achsenteilstriche auf der x-Achse
#' @param max_label_count Der x-Achsenwert für das letzte, maximale Label
#' @param labs_nth_count Schrittlänge für die großen x-Achsenteilstriche mit Labels
#' @param title_x_m Titel für x-Achse der linken, männlichen Hälfte; wird als 
#'                  `title_x` an die Funktion `draw_pyramid_side()` weitergegeben
#' @param title_x_m Titel für x-Achse der rechten, weiblichen Hälfte; wird als 
#'                  `title_x` an die Funktion `draw_pyramid_side()` weitergegeben
#' @param title_x_centre Haupttitel in der Mitte der x-Achse
#' @param title_x_centre_pos Position des Hauptitels (muss oft leicht verschoben 
#'                           werden je nach Länge um mittig zu erscheinen)
#'                           
#' @return ggplot Objekt (Pyramide)

draw_pyramid <- function(data, title = NULL, outline = TRUE, colors, 
                         axis_family = "Iosevka Slab", limit_age = 90, 
                         steps_age = 1, labs_nth_age = 5, label_age = "\nJahre", 
                         limit_count = 800, steps_count = 250, max_label_count = 750, 
                         labs_nth_count = 1,title_x_m = NULL, title_x_f = NULL, 
                         title_x_centre = NULL, title_x_centre_pos = .65) { 
  
  ##-------------------------- Zentrierte Achse --------------------------------
  ## Die Achse wird als eigener Plot generiert, um sicherzustellen, dass die 
  ## Weite beider Pyramidenhälften beim Zusammenfügen identisch ist.
  
  ## wenn ein Label mit Zeilenumbruch (\n) zur y-Achse hinzugefügt wird, wird
  ## die Position dieses Labels adjustiert
  if (stringr::str_detect(label_age, "\n")) {
    vjust <- c(rep(.5, limit_age / steps_age), .8)
  } else {
    vjust <- .5
  }
  
  p_axis <-
    ggplot2::ggplot(data, ggplot2::aes(y = Alter)) +
    ggplot2::geom_blank() +
    
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      breaks = seq(0, limit_age, by = steps_age),
      labels = function(y) every_nth(y, labs_nth_age, inverse = TRUE, label = label_age),
      limits = c(0, limit_age)) +
    
    ggplot2::labs(x = title_x_centre, y = NULL, title = title) +
    
    ## spezielles Theme für die y-Achse
    ggplot2::theme_void() +
    
    ggplot2::theme(
      axis.title = ggtext::element_markdown(
        family = "BundesSans Web", size = 14, color = "black", 
        margin = ggplot2::margin(t = 4, b = 0), hjust = title_x_centre_pos
      ),
      axis.text.y = ggplot2::element_text(
        family = axis_family, size = 7, color = "grey25", lineheight = .95,
        margin = ggplot2::margin(0, 2.5, 0, 2.5), hjust = .5, vjust = vjust
      ),
      plot.title = ggplot2::element_text(
        family = "BundesSans Web", size = 11, color = "black", 
        face = "bold", margin = ggplot2::margin(b = 7), hjust = 0.5
      ),
      plot.title.position = "plot"
    )
  
  ##------------------------- Männliche Hälfte ---------------------------------
  
  p_male <- draw_pyramid_side(
    data = data, side = "left", outline = outline, colors = colors, 
    title_x = title_x_m, axis_family = axis_family, 
    limit_age = limit_age, steps_age = steps_age, labs_nth_age = labs_nth_age, 
    limit_count = limit_count, max_label_count = max_label_count, 
    steps_count = steps_count, labs_nth_count = labs_nth_count
  )
  
  ##-------------------------- Weibliche Hälfte ---------------------------------
  
  p_female <- draw_pyramid_side(
    data = data, side = "right", outline = outline, colors = colors, 
    title_x = title_x_f, axis_family = axis_family, 
    limit_age = limit_age, steps_age = steps_age, labs_nth_age = labs_nth_age, 
    limit_count = limit_count, max_label_count = max_label_count, 
    steps_count = steps_count, labs_nth_count = labs_nth_count
  )
  
  ##------------------------- Gesamte Pyramide ---------------------------------
  ## Kombinieren der beoden Pyramidenhälften `p_male` und `p_female`
  
  p_pyramid <- 
    patchwork::wrap_plots(p_male, p_axis, p_female) +
    patchwork::plot_layout(nrow = 1, widths = c(limit_count, 0, limit_count))
  
  return(p_pyramid)
}



#' Erstellen eines 3-Pyramiden-Layouts
#' 
#' Ausfertigung und Kombination dreier Pyramiden in einem Gesamtlayout. Die drei
#' Die Pyramiden werden mit `draw_pyramid()` erstellt und nutzen standardmäßig
#' Datensätze gespeichert in Objekten namens `data_91`, `data_06` und `data_21`.
#' Folglich sind die voreingestellten Einzeltitel `"1991"`, `"2006"` und `"2021"`.
#' 
#' Diese Funktion ist die Hauptfunktion, die zur Erstellung der Pyramiden 
#' aufgerufen wird und im Hintergrund die Funktion `draw_pyramid()`, die wiederum
#' die Subfunktion `draw_pyramid_side()` nutzt. Die Voreinstellungen sind so 
#' gewählt, dass lediglich der jweilige Datensatz und dazugehörige Titel, die
#' Farben sowie die x-Achsentitel überschrieben werden können.
#' 
#' @param data_1 Der mit der Funktion `prep_data_pyramid()` vorbereitete Datensatz
#'               für die erste, links platzierte Pyramide
#' @param data_2 Der mit der Funktion `prep_data_pyramid()` vorbereitete Datensatz
#'               für die zweite, mittig platzierte Pyramide
#' @param data_3 Der mit der Funktion `prep_data_pyramid()` vorbereitete Datensatz
#'               für die dritte, rechts platzierte Pyramide
#' @param title_1 Der Titel für die erste, links platzierte Pyramide
#' @param title_2 Der Titel für die zweite, mittig platzierte Pyramide
#' @param title_3 Der Titel für die dritte, rechts platzierte Pyramide
#' @param colors Farbvektor für die einzelnen Gruppen. Dieser wird einheitlich
#'               für alle Pyramiden genutzt; eine Spezifikation je Pyramide ist
#'               *nicht* möglich
#' @param title_m Titel für x-Achse der linken, männlichen Hälfte; wird als 
#'                `title_x_m` an die Funktion `draw_pyramid_side()` weitergegeben
#' @param title_f Titel für x-Achse der rechten, weiblichen Hälfte; wird als 
#'                `title_x_f` an die Funktion `draw_pyramid_side()` weitergegeben
#'                
#' @return Liste von drei ggplot Objekten (Pyramiden)                

draw_set_pyramids <- function(data_1 = data_91, data_2 = data_06, data_3 = data_21, 
                              title_1 = "1991", title_2 = "2006", title_3 = "2021", 
                              colors = colors, title_m = title_x_m, title_f = title_x_f) {
  
  p1 <- draw_pyramid(data = data_1, title = title_1, colors = colors, 
                     title_x_m = title_m, title_x_f = title_f)
  
  p2 <- draw_pyramid(data = data_2, title = title_2, colors = colors) 
  
  p3 <- draw_pyramid(data = data_3, title = title_3, colors = colors)
  
  return(list(p1, p2, p3))
  
}



#' Erstellen eines 2-Pyramiden-Layouts
#' #' 
#' Ausfertigung und Kombination zweier Pyramiden in einem Gesamtlayout. Dies 
#' ist eine Variante der `draw_set_pyramids()` Funktion für die Ausnahme bei dem 
#' Thema "Migrationshintergrund", welches nur zwei Zeitpunkte darstellt.
#' Die Pyramiden werden mit `draw_pyramid()` erstellt und nutzen standardmäßig 
#' Datensätze gespeichert in Objekten namens `data_05` und `data_21`.
#' Folglich sind die voreingestellten Einzeltitel `"2005"` und `"2021"`.
#' 
#' Diese Funktion ist die Hauptfunktion (für das Thema "Migrationshintergrund"),
#' die zur Erstellung der Pyramiden aufgerufen wird und im Hintergrund die 
#' Funktion `draw_pyramid()`, die wiederum die Subfunktion `draw_pyramid_side()` 
#' nutzt. Die Voreinstellungen sind so gewählt, dass lediglich der jweilige 
#' Datensatz und dazugehörige Titel, die Farben sowie die x-Achsentitel 
#' überschrieben werden können.
#' 
#' @param data_1 Der mit der Funktion `prep_data_pyramid()` vorbereitete Datensatz
#'               für die erste, links platzierte Pyramide
#' @param data_2 Der mit der Funktion `prep_data_pyramid()` vorbereitete Datensatz
#'               für die zweite, rechts platzierte Pyramide
#' @param title_1 Der Titel für die erste, links platzierte Pyramide
#' @param title_2 Der Titel für die zweite, rechts platzierte Pyramide
#' @param colors Farbvektor für die einzelnen Gruppen. Dieser wird einheitlich
#'               für alle Pyramiden genutzt; eine Spezifikation je Pyramide ist
#'               *nicht* möglich
#' @param title_m Titel für x-Achse der linken, männlichen Hälfte; wird als 
#'                `title_x_m` an die Funktion `draw_pyramid_side()` weitergegeben
#' @param title_f Titel für x-Achse der rechten, weiblichen Hälfte; wird als 
#'                `title_x_f` an die Funktion `draw_pyramid_side()` weitergegeben
#'                
#' @return Liste von zwei ggplot Objekten (Pyramiden)

draw_set_pyramids_2 <- function(data_1 = data_05, data_2 = data_21, 
                                title_1 = "2005", title_2 = "2021", 
                                colors = colors, title_m = title_x_m, title_f = title_x_f) {
  
  p1 <- draw_pyramid(data = data_1, title = title_1, colors = colors, 
                     title_x_m = title_m, title_x_f = title_f)
  
  p2 <- draw_pyramid(data = data_2, title = title_2, colors = colors) 
  
  return(list(p1, p2))
  
}



#' Speichern der Pyramiden + Test auf Verträglichkeit mit Farbblindheit
#' 
#' Zusammenfügen sowie Speichern der Einzelpyramiden inklusive der Möglichkeit
#' einen Gesamtitel zu setzen. Die zwei oder drei Pyramiden müssen mit 
#' `draw_set_pyramids()` bzw. `draw_set_pyramids_2()` erstellt worden sein. 
#' Die Funktion kreiiert verschiedene Layouts für unterschiedliche Zwecke.
#' Die svg Datei wird im Anschluss in einem Vektorgrafiktool gesetzt und mit dem
#' Zeitstrahl sowie Annotierungen versehen; daher wird bei dieser Version auch
#' die Legende entfernt. Die pdf und png Dateien sind als fertige Pyramiden
#' (ohne Annotierung und Zeitstrahl) vorgesehen und besitzen daher eine Legende.
#' 
#' Zusätzlich wird eine Simulation für verschiedene Arten von Farbblindheit 
#' vorgenommen. Diese umfasst Deuteranopie und Protanopie (sowie Graustufen, 
#' welche zu vernachlässigen sind in unserem Fall). Das Ergebnis wird mit der  
#' Namenserweiterung "_cvd" als png Datei gespeichert.
#' 
#' Die Bilddateien werden in einem Unterordner namens `plots` erstellt.
#' 
#' @param plots Eine Liste von zwei oder drei gpglot Objekten (Pyramiden), 
#'              welche mit der Funktion `draw_set_pyramids()` bzw. 
#'              `draw_set_pyramids_2()` erstellt wurden.
#' @param spacing Abstand zwischen den Pyramiden, relativ zur Weite einer Pyramide
#' @param main_title Übergreifender Titel für das Gesamtlayout
#' @param name Name für die Bidldateien
#' @param margin Rand um das Gesamtlayout. Standardmäßig ohne Rand, da das 
#'              finale Layout anschließend erstellt wird und dann im Bericht 
#'              entsprechend gesetzt wird

draw_all_pyramids <- function(plots = list(), spacing = .075, main_title = NULL,
                              name, margin = ggplot2::margin(0, 0, -5, 0)) { 
  
  if(!length(plots) %in% 2:3) stop("Die Funktion ist nur für 2 oder 3 Abibldungen geeignet.")
  
  ## create folder directory if necessary
  suppressWarnings(dir.create(here::here("plots"), showWarnings = FALSE))
  
  if (length(plots) == 3) {
    
    panel <- 
      patchwork::wrap_plots(plots[[1]], plot_spacer(), plots[[2]], plot_spacer(), plots[[3]]) +
      patchwork::plot_layout(guides = "collect", widths = c(1, spacing, 1, spacing, 1),)
    
  } else { ## length(plots) == 2
    
    panel <- 
      patchwork::wrap_plots(plot_spacer(), plots[[1]], plot_spacer(), plots[[2]], plot_spacer()) +
      patchwork::plot_layout(guides = "collect", widths = c(spacing, 1, spacing, 1, spacing))
  }
  
  p_edit <- 
    panel +
    plot_annotation(
      title = main_title,
      theme = theme(
        legend.position = "none",
        plot.margin = margin,
        plot.title = element_blank()
      )
    )

  ggsave(plot = p_edit, filename = here::here("plots", paste0(name, ".svg")),
         width = 240, height = 132, units = "mm")
  
  
  p_fix <- 
    panel + 
    plot_annotation(
      title = main_title,
      theme = theme(
        legend.position = "bottom",
        legend.justification = "left",
        plot.margin = margin,
        plot.title.position = "plot",
        plot.title = ggtext::element_textbox_simple(
          margin = ggplot2::margin(5, 0, 5, 0), hjust = .5, halign = .5,
          family = "BundesSans Web", face = "bold", size = 16
        )
      )
    )
  
  ggsave(plot = p_fix, filename = here::here("plots", paste0(name, ".pdf")),
         width = 240, height = 150, units = "mm", device = cairo_pdf)
  
  ggsave(plot = p_fix, filename = here::here("plots", paste0(name, ".png")), 
         width = 240, height = 150, units = "mm", dpi = 300, bg = "white")
  
  p_cvd <- colorBlindness::cvdPlot(p_fix)
  
  ggsave(plot = p_cvd, filename = here::here("plots", paste0(name, "_cvd.png")), 
         width = 480, height = 300, units = "mm", dpi = 150, bg = "white")
  
  return(p_edit)
  
}
