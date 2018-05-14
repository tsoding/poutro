(function() {
    // TODO(#11): Preview fps and frameCount are hardcoded
    let fps = 30;
    let frameCount = 151;
    let previewElement = document.getElementById("preview");

    function showFrame(n) {
        previewElement.src = "out/" + n + ".svg";
        setTimeout(showFrame,  1000.0 / fps, (n + 1) % frameCount);
    }

    showFrame(0);
})();
