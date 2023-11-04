//See: http://www.css-101.org/articles/ken-burns_effect/css-transition.php
(function(){
  document.getElementById('slideshow').getElementsByTagName('img')[0].className = "fx";
  // this calls the kenBurns function every 4 seconds
  // you can increase or decrease this value to get different effects
  window.setInterval(kenBurns, 6000);
  var images          = document.getElementById('slideshow').getElementsByTagName('img'),
      numberOfImages  = images.length,
      i               = 1;

  function kenBurns() {
    if(i==numberOfImages){ i = 0;}
    images[i].className = "fx";
    //window.alert(images[i].alt);
    document.getElementById("date").innerHTML = document.getElementById("date").innerHTML + " - " + images[i].alt;

    // we can't remove the class from the previous element or we'd get a bouncing effect so we clean up the one before last
    // (there must be a smarter way to do this though)
    if(i===0){ images[numberOfImages-2].className = "";}
    if(i===1){ images[numberOfImages-1].className = "";}
    if(i>1){ images[i-2].className = "";}
    i++;
  }
})();
// toggle fullscreen
var elem = document.documentElement;
function togglefullscreen() {
  var isFullScreen = document.fullScreen ||
      document.mozFullScreen ||
      document.webkitIsFullScreen || (document.msFullscreenElement != null);
  if (isFullScreen) {
      console.log('fullScreen!');
      if (document.exitFullscreen) {
        document.exitFullscreen();
      } else if (document.webkitExitFullscreen) {/* Safari */
        document.webkitExitFullscreen();
      } else if (document.msExitFullscreen)     {/* IE11 */
        document.msExitFullscreen();
      } else if (document.mozCancelFullScreen)  {/* IE11 */
        document.mozCancelFullScreen();
      }
  } else {
      if (elem.requestFullscreen) {
        elem.requestFullscreen();
      } else if (elem.webkitRequestFullscreen) {/* Safari */
        elem.webkitRequestFullscreen();
      } else if (elem.msRequestFullscreen)     {/* IE11 */
        elem.msRequestFullscreen();
      } else if (elem.mozRequestFullScreen)    {/* seamonkey */
        elem.mozRequestFullScreen();
      }
  }
}
