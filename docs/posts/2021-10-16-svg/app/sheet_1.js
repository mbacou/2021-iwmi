// !preview r2d3 data=NULL

var pal = ['#3C8DBC','#DD4B39','#00A65A','#00C0EF','#F39C12','#0073B7','#001F3F',
  '#39CCCC','#3D9970','#01FF70','#FF851B','#F012BE','#605CA8','#D81B60','#111111','#D2D6DE'];

// Test interactions
function handleMouseOver(d, i) {
   d3.select(this)
   .attr("fill-opacity", 0.5);
}

function handleMouseOut(d, i) {
  d3.select(this)
  .attr("fill-opacity", 1);
}

const obj = svg
  .insert("svg:g")
  .attr("class", "sheet_1");

obj.selectAll("svg").remove();

// Init external design
d3.xml("sheet_1_edited.svg")
  .then(d => {
  obj.node().append(d.documentElement);

  obj
    .selectAll("rect")
      .on("mouseover", handleMouseOver)
      .on("mouseout", handleMouseOut)
      .on("click", function() {
        Shiny.setInputValue(
          "bar_clicked", {
            "id" : d3.select(this).attr("id"),
            "data" : d3.select(this).attr("d")
            }, {priority: "event"}
          );
        console.log(d3.select(this).attr("d"));
      });

});


// Rendering
r2d3.onRender(function(data, svg, width, height, options) {

var root = svg.select(".sheet_1").select("svg");

  root
    .selectAll("rect")
    .data(data)
    //.attr("height", d => 1000*d.value)
    .attr("d", d => d.value);

  root
    .selectAll("text")
    .data(data)
    .text(d => d3.format("(.2f")(d.value));

});
