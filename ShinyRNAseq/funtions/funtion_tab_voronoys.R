tab_voronoys <- function(texto, texto2, texto3, cor, icon, id){
  HTML(paste0('<a id="', id,'" href="#" class="action-button">
                  <div class = "voronoys-block" style = "width:80%;height: 130px;background-color:', cor, ';"> 
                  <span class = "name" style = "font-size:20px">', texto, '</span>
                  <span class = "name2">', texto2, '</span>
                  <span class = "name3">', texto3, '</span>
                  <div class="img_block">
                    <div class="img_block_conteiner">',
                      icon,
                    '</div>
                  </div>
              </div></a>'))
}

tab_voronoys1 <- function(texto, texto2, texto3, cor, icon, id){
  HTML(paste0('<a id="', id,'" href="#" class="action-button">
                  <div class = "voronoys-block" style = "width:80%;height: 130px;background-color:', cor, ';"> 
                  <span class = "name" style = "font-size:20px">', texto, '</span>
                  <span class = "name2">', texto2, '</span>
                  <span class = "name3">', texto3, '</span>
                  <div class="img_block">
                    <div class="img_block_conteiner">',
                      icon,
                    '</div>
                  </div>
              </div></a>'))
}