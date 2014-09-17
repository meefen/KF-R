<style>

// .node {
//   stroke: #fff;
//   stroke-width: 1.5px;
// }

// .node text {
//   pointer-events: none;
//   font: 10px sans-serif;
// }

// .link {
//   stroke: #999;
//   stroke-opacity: .6;
// }

.link {
  stroke: #ccc;
}

.node text {
  pointer-events: none;
  font: 10px sans-serif;
}

</style>

<script src="http://d3js.org/d3.v3.min.js"></script>
<script type="text/javascript">
var networkOutputBinding = new Shiny.OutputBinding();
$.extend(networkOutputBinding, {
  find: function(scope) {
    return $(scope).find('.shiny-network-output');
  },
  renderValue: function(el, data) {

    //format nodes object
    var nodes = new Array();
    for (var i = 0; i < data.names.length; i++){
      nodes.push({"name": data.names[i]})
    }

    var width = 800;
    var height = 400;

    var link = data.links
    var force = d3.layout.force()
    .nodes(nodes)
    .links(link)
    .gravity(.05)
    .distance(100)
    .charge(-100)
    .linkDistance(50)
    .size([width, height])
    .start();
    
    //remove the old graph
    var svg = d3.select(el).select("svg");      
    svg.remove();
    
    $(el).html("");
    
    //append a new one
    svg = d3.select(el).append("svg");
    
    svg.attr("width", width)
    .attr("height", height);

    var link = svg.selectAll("line.link")
    .data(link)
    .enter().append("line")
    .attr("class", "link")
    .style("stroke-width", function(d) { return Math.sqrt(d.value); });

    var node = svg.selectAll(".node")
    .data(nodes)
    .enter().append("g")
    .attr("class", "node")
      // .attr("r", 5)
      //.style("fill", function(d) { return color(d.group); })
      .call(force.drag);

      node.append("image")
      .attr("xlink:href", "https://github.com/favicon.ico")
      .attr("x", -8)
      .attr("y", -8)
      .attr("width", 16)
      .attr("height", 16);

      node.append("text")
      .attr("dx", 12)
      .attr("dy", ".35em")
      .text(function(d) { return d.name; });


      force.on("tick", function() {
        link.attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; });

        node.attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });

      // node.attr("cx", function(d) { return d.x; })
      //   .attr("cy", function(d) { return d.y; });
    });

    }
  });
Shiny.outputBindings.register(networkOutputBinding, 'trestletech.networkbinding');

</script>
