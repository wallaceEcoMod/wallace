// Scroll to the bottom of the log window.
var oldContent = null;
window.setInterval(function() {
  var elem = document.getElementById('wallaceLog');
  if (oldContent != elem.innerHTML){
    scrollToBottom();
  }
  oldContent = elem.innerHTML;  
}, 300);

function scrollToBottom(){
  var elem = document.getElementById('wallaceLog');
  elem.scrollTop = elem.scrollHeight;
}
