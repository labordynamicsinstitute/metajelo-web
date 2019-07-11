window.handleFileEdit= function handleFileEdit() {
    var xmlTxt = document.getElementById('metajelo_xml_entry').value;
    Metajelo.renderRecord('metajelo_root')(xmlTxt)();
};
window.handleFileSelect = function handleFileSelect(evt) {
    var files = evt.target.files; // FileList object

    // use the 1st file from the list
    file = files[0];

    var reader = new FileReader();

    // Closure to capture the file information.
    reader.onload = (function(theFile) {
        return function(ev) {
            Metajelo.renderRecord("metajelo_root")(ev.target.result)();
        };
    })(file);

    // Read in the image file as a data URL.
    reader.readAsText(file);
};

window.doDownload = function doDownload(str) {
    function dataUrl(data) {
        return "data:x-application/xml;charset=utf-8," + escape(data);
    }
    var downloadLink = document.createElement("a");
    downloadLink.href = dataUrl(str);
    downloadLink.download = "metajelo.xml";

    document.body.appendChild(downloadLink);
    downloadLink.click();
    document.body.removeChild(downloadLink);
}

document.getElementById('metajelo_upload')
        .addEventListener('change', handleFileSelect, false);
document.getElementById('metajelo_xml_entry')
        .defaultValue = document.getElementById("autogen_mj_xml").innerText;
