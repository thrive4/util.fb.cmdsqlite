const element = document.getElementById('darkmodeswitch');
element.addEventListener("darkmodeswitch", switchtheme);

if ( window.localStorage.getItem('theme') === 'dark') {
  document.body.classList.add('dark');
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
   var data = document.getElementsByClassName("footer");
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("footerdarkmode")
   }
   var data = document.getElementsByClassName("nav-item");
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("nav-itemdarkmode")
   }
   var data = document.getElementsByClassName("nav-right");
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("nav-rightdarkmode")
   }
   var data = document.querySelectorAll('#myTable tr:nth-child(odd)');
   for (i = 0; i < data.length; i++) {
     data[i].classList.toggle("trdarkmode")
   }
}

// todo add svg solution
if ( window.localStorage.getItem('theme') === 'dark') {
   //var favicon = document.querySelector('link[rel="shortcut icon"]'); // get favicon-192.png element
   //favicon.href = 'images/favicondark.jpg';
   //largeFavicon.href = '/assets/images/favicon-dark-192.png';
} else {
   //favicon.href = 'images/favicon.jpg';
   //largeFavicon.href = '/assets/images/favicon-192.png';
}
