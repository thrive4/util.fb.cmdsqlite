// dark mode toggle
function switchtheme() {
   document.body.classList.toggle('dark');
   var data = document.getElementsByClassName("card");
   for (i = 0; i < data.length; i++) {
     //data[i].style.fontSize = "30px";
     //data[i].style.background = "green";
     data[i].classList.toggle("carddarkmode")
   }
   var data = document.getElementsByClassName("nav");
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("navdarkmode")
   }
   var data = document.getElementsByClassName("nav-item");
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("nav-itemdarkmode")
   }
   var data = document.getElementsByClassName("nav-right");
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("nav-rightdarkmode")
   }
   var data = document.getElementsByClassName("svglight");
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("svgdarkmode")
   }
   var data = document.getElementsByClassName("footer");
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("footerdarkmode")
   }
   window.localStorage.setItem('theme', document.body.classList.contains('dark') ? 'dark' : 'light');
   // debug feedback
   //document.getElementById("result").innerHTML =  window.localStorage.getItem('theme');
}

const element = document.getElementById('darkmodeswitch');
element.addEventListener("darkmodeswitch", switchtheme);

if ( window.localStorage.getItem('theme') === 'dark') {
   document.body.classList.add('dark');
   var data = document.getElementsByClassName("card");
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("carddarkmode")
   }
   var data = document.getElementsByClassName("nav");
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("navdarkmode")
   }
   var data = document.getElementsByClassName("nav-item");
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("nav-itemdarkmode")
   }
   var data = document.getElementsByClassName("nav-right");
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("nav-rightdarkmode")
   }
   var data = document.getElementsByClassName("svglight");
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("svgdarkmode")
   }
   var data = document.getElementsByClassName("footer");
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("footerdarkmode")
   }
   var data = document.getElementsByClassName("trlight");
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("trdarkmode")
   }
}

// sort table data
// todo figure out why these functions can not be placed in lib.js
// in lib.js music dataview no longer sorts...
/**
 * sortable 1.0
 * Makes html tables sortable, ie9+
 * Styling is done in css.
 * Copyleft 2017 Jonas Earendel
 * This is free and unencumbered software released into the public domain.
 * more info see https://github.com/tofsjonas/sortable/blob/main/sortable.js
*/

document.addEventListener('click', function (e) {
  var down_class  = ' dir-d '
  var up_class    = ' dir-u '
  var regex_dir   = / dir-(u|d) /
  var regex_table = /\bsortable\b/
  var element     = e.target

  function reClassify(element, dir) {
    element.className = element.className.replace(regex_dir, '') + dir
  }

  function getValue(element) {
    // If you aren't using data-sort and want to make it just the tiniest bit smaller/faster
    // comment this line and uncomment the next one
    return element.getAttribute('data-sort') || element.innerText
  }

  if (element.nodeName === 'TH') {
    try {
      var tr = element.parentNode
      // var table = element.offsetParent; // Fails with positioned table elements
      // this is the only way to make really, really sure. A few more bytes though... 😡
      var table = tr.parentNode.parentNode
      if (regex_table.test(table.className)) {
        var column_index
        var nodes = tr.cells
        // reset thead cells and get column index
        for (var i = 0; i < nodes.length; i++) {
          if (nodes[i] === element) {
            column_index = i
          } else {
            reClassify(nodes[i], '')
          }
        }
        var dir = down_class
        // check if we're sorting up or down, and update the css accordingly
        if (element.className.indexOf(down_class) !== -1) {
          dir = up_class
        }
        reClassify(element, dir)
        // extract all table rows, so the sorting can start.
        var org_tbody = table.tBodies[0]
        // get the array rows in an array, so we can sort them...
        var rows = [].slice.call(org_tbody.rows, 0)
        var reverse = dir === up_class
        // sort them using custom built in array sort.
        rows.sort(function (a, b) {
          var x = getValue((reverse ? a : b).cells[column_index])
          var y = getValue((reverse ? b : a).cells[column_index])
          // var y = (reverse ? b : a).cells[column_index].innerText
          // var x = (reverse ? a : b).cells[column_index].innerText
          return isNaN(x - y) ? x.localeCompare(y) : x - y
        })
        // Make a clone without content
        var clone_tbody = org_tbody.cloneNode()
        // Build a sorted table body and replace the old one.
        while (rows.length) {
          clone_tbody.appendChild(rows.splice(0, 1)[0])
        }
        // And finally insert the end result
        table.replaceChild(clone_tbody, org_tbody)
      }
    } catch (error) {
      // console.log(error)
    }
  }
})

// group and filter table data
// via https://www.w3schools.com/howto/howto_js_filter_table.asp
function searchandfilter() {
  var input, filter, table, tr, td, i, txtValue;
  input  = document.getElementById("srinput");
  filter = input.value.toUpperCase();
  table  = document.getElementById("datatable");
  tr     = table.getElementsByTagName("tr");

  // Loop through all table rows, and hide those who don't match the search query
  for (i = 0; i < tr.length; i++) {
    td = tr[i].getElementsByTagName("td")[window.localStorage.getItem('tdelement')];
    // todo allow for grouping by column
    //td = tr[i].getElementsByTagName("td")[0];
    if (td) {
      txtValue = td.textContent || td.innerText;
      if (txtValue.toUpperCase().indexOf(filter) > -1) {
        tr[i].style.display = "";
      } else {
        tr[i].style.display = "none";
      }
    }
  }
}
