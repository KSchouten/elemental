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
  if (e.keyCode === 18){
    console.log("Light it up!")
    $('.layout').toggleClass('layout-visible')
    $('.layout').toggleClass('layout-invisible')
  }
}