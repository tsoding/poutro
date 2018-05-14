(function() {
    let previewElement = document.getElementById("preview");

    function showFrame(n) {
        previewElement.src = "out/" + n + ".svg";
        setTimeout(showFrame,  1000.0 / meta.fps, (n + 1) % meta.frameCount);
    }

    showFrame(0);
})();
