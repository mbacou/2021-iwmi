/*
 YouTube Audio Embed
 --------------------

 Author: Amit Agarwal
 Web: http://www.labnol.org/?p=26740
*/

function onYouTubeIframeAPIReady(){var e=document.getElementById("youtube-audio"),t=document.createElement("img");t.setAttribute("id","youtube-icon"),t.style.cssText="cursor:pointer;cursor:hand",e.appendChild(t);var a=document.createElement("div");a.setAttribute("id","youtube-player"),e.appendChild(a);var o=function(e){var a=e?"audio-player.png":"audio-player.png";t.setAttribute("src","www/"+a)};

e.onclick=function(){r.getPlayerState()===YT.PlayerState.PLAYING||r.getPlayerState()===YT.PlayerState.BUFFERING?(r.pauseVideo(),o(!1)):(r.playVideo(),o(!0))};var r=new YT.Player("youtube-player",{height:"0",width:"0",videoId:e.dataset.video,playerVars:{autoplay:e.dataset.autoplay,loop:e.dataset.loop},events:{onReady:function(e){r.setPlaybackQuality("small"),o(r.getPlayerState()!==YT.PlayerState.CUED)},onStateChange:function(e){e.data===YT.PlayerState.ENDED&&o(!1)}}})}

var listID = 'PLMEI8z_d3EZGRnn8nGh3f6A7zwT4wDmVU';
var numberOfVideos = 200;
var randomizer = Math.floor(Math.random() * numberOfVideos + 1);

document.writeln('<div class="quarto-video" data-video="videoseries?listType=playlist&list=' + listID + '&index=' + randomizer + '" data-autoplay="0" data-loop="0" id="youtube-audio"></div>');
