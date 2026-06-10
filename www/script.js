function get_layout(page){
  var layout = [];
  var rows = $('#'+ page + '> .page-layout-row');
  for (i=0; i < rows.length; i++){
    var row = [];
    var cols = rows.eq(i).children(".page-layout-column");
    for (j = 0; j < cols.length; j++){
      var col = [cols.eq(j).attr('size')];
      var boxes = cols.eq(j).find('> div > .box');
      boxes.each(function(){
        col.push($(this).attr('id'));
      })
      row.push(col);
    }
    layout.push(row);
    //console.log(JSON.stringify(layout))
  }
  return JSON.stringify(layout);
}

document.addEventListener("keydown", logKey)

function logKey(e){
  console.log(e)
  if (e.key === '\\'){ //Alt = 18, does not work well on Windows
    console.log("Light it up!")
    $('.layout-column').toggleClass('layout-visible')
    //$('.layout').toggleClass('layout-invisible')
    
  }

}

document.addEventListener("bslib.card", communicateFullScreenState)

function communicateFullScreenState(e){
  console.log(e)
  //e.detail.fullScreen)
  
  var id = $(e.target).children().eq(0).children().eq(1).attr('id')
  Shiny.setInputValue(id+'_full_screen', e.detail.fullScreen)
}

function move_module(evt){
  //console.log(evt.from.id);
  //evt.from.id
  //evt.oldIndex
  
  //evt.to.id
  //evt.newIndex
  
  var tab_title = $(evt.item).children().html();
  var tab_id = $(evt.item).children().attr('data-value')
  var sibling_tab_id = $($($('#'+evt.from.id).children()[0]).children()[0]).attr('data-value')
  var place_before_id = $($($(evt.to).children()[evt.newIndex+1]).children()[0]).attr('data-value')

  if (evt.newIndex !== evt.oldIndex | evt.from.id !== evt.to.id){
    Shiny.setInputValue('move_module', {'from_tile': evt.from.id, 'from_index': evt.oldIndex, 'to_tile': evt.to.id, 'to_index': evt.newIndex, 'tab_title': tab_title, 'tab_id': tab_id, 'sibling_tab_id': sibling_tab_id, 'place_before_id': place_before_id})
  }
  if (evt.from.id !== evt.to.id){
    evt.item.remove()
  }

}
