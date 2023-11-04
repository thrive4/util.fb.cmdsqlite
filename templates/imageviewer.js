// generate image overlay data
function imageoverlay() {
    let text           = "";
    let cnt            = 1;
    // get json data
    url = `imageviewer.json`;
    request = new XMLHttpRequest();
    request.open('GET', url, true);
    // work around xml parsing error in firefox, etc
    request.overrideMimeType("text/html");
    request.onload = function () {
        text += '  <span class="playslide">';
        text += '        <a href="slide.html" style="text-decoration: none;" target="_blank">';
        text += '            <svg class="svglight" viewBox="0 0 32 3" height="16px" width="16px" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">';
        text += '            <path d="M1,14c0,0.547,0.461,1,1,1c0.336,0,0.672-0.227,1-0.375L14.258,9C14.531,8.867,15,8.594,15,8s-0.469-0.867-0.742-1L3,1.375  C2.672,1.227,2.336,1,2,1C1.461,1,1,1.453,1,2V14z"/>';
        text += '            </svg>';
        text += '        </a>&nbsp;&nbsp;';
        text += '  </span>';
        text += '   <p id="time"></p>';
        text += '   <p id="date"></p>';
        text += '    <div class="w3-content w3-display-container">';

        var objct = JSON.parse(this.response);
        Object.entries(objct).forEach((entry) => {
            const [key, value] = entry;
            //window.alert(`${key}${value.name}`);
            text += '        <div class="w3-display-container mySlides">';
            text += '         <img id="lazy" class="w3-animate-left ovimage" src="' + value.href + '">';
            text += '         <div class="w3-display-bottomleft-stretch w3-container w3-padding-8 w3-black">';
            text += value.name;
            text += '          </div>';
            text += '        </div>';
        });

        text += '        <div class="ovthumbbox">';
        Object.entries(objct).forEach((entry) => {
            const [key, value] = entry;
            //window.alert(`${key}${value.name}`);
            text += '        <img id="lazy" class="demo w3-opacity w3-hover-opacity-off ovthumb" src="' + value.href + '" onclick="currentDiv(' + cnt + ')">';
            cnt += 1;
        });

        text += '        </div>';
        text += '        <!-- image navigation left and right -->';
        text += '        <div class="w3-text-white w3-display-middle" style="top:350px;width:90%">';
        text += '             <div class="w3-left w3-hover-text-khaki"  onclick="plusDivs(-1)">&#10094;</div>';
        text += '             <div class="w3-right w3-hover-text-khaki" onclick="plusDivs(1)">&#10095;</div>';
        text += '        </div>';
        document.getElementById("imageoverlay").innerHTML = text;
        //window.alert(text);

    };
    request.send();
    // clean up
    text = "";
}

imageoverlay();

// ghetto lazyload images for imageviewer drawback is called for every click
// via https://stackoverflow.com/questions/67988689/modify-the-src-to-data-src-javascript Junaid Hamza
document.addEventListener("click", getimagesviewer);
function getimagesviewer() {
  if (document.getElementById('myModal').style.display == 'block') {
    var imgs =  document.getElementsByClassName('w3-animate-left');
    imgload(imgs);
    // thumbs
    var imgs =  document.getElementsByClassName('demo');
    imgload(imgs);
  } else {
    var imgs =  document.getElementsByClassName('w3-animate-left');
    imglazy(imgs);
    // thumbs
    var imgs =  document.getElementsByClassName('demo');
    imglazy(imgs);
  }
}

function imgload(imgs){
    for (var i = 0; i < imgs.length; i++) {
      if (imgs[i].getAttribute('data-src')) {
        imgs[i].setAttribute('src', imgs[i].getAttribute('data-src'));
        imgs[i].removeAttribute('data-src');
      }
    }
}

function imglazy(imgs){
    for (var i = 0; i < imgs.length; i++) {
      if (imgs[i].getAttribute('src')) {
        imgs[i].setAttribute('data-src', imgs[i].getAttribute('src'));
        imgs[i].removeAttribute('src');
      }
    }
}
