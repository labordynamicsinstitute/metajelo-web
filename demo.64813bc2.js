parcelRequire=function(e,r,t,n){var i,o="function"==typeof parcelRequire&&parcelRequire,u="function"==typeof require&&require;function f(t,n){if(!r[t]){if(!e[t]){var i="function"==typeof parcelRequire&&parcelRequire;if(!n&&i)return i(t,!0);if(o)return o(t,!0);if(u&&"string"==typeof t)return u(t);var c=new Error("Cannot find module '"+t+"'");throw c.code="MODULE_NOT_FOUND",c}p.resolve=function(r){return e[t][1][r]||r},p.cache={};var l=r[t]=new f.Module(t);e[t][0].call(l.exports,p,l,l.exports,this)}return r[t].exports;function p(e){return f(p.resolve(e))}}f.isParcelRequire=!0,f.Module=function(e){this.id=e,this.bundle=f,this.exports={}},f.modules=e,f.cache=r,f.parent=o,f.register=function(r,t){e[r]=[function(e,r){r.exports=t},{}]};for(var c=0;c<t.length;c++)try{f(t[c])}catch(e){i||(i=e)}if(t.length){var l=f(t[t.length-1]);"object"==typeof exports&&"undefined"!=typeof module?module.exports=l:"function"==typeof define&&define.amd?define(function(){return l}):n&&(this[n]=l)}if(parcelRequire=f,i)throw i;return f}({"lgAh":[function(require,module,exports) {
window.handleFileEdit=function(){var e=document.getElementById("metajelo_xml_entry").value;Metajelo.renderRecord("metajelo_root")(e)()},window.handleFileSelect=function(e){var t=e.target.files;file=t[0];var l=new FileReader;l.onload=(file,function(e){Metajelo.renderRecord("metajelo_root")(e.target.result)()}),l.readAsText(file)},window.doDownload=function(e){var t=document.createElement("a");t.href="data:x-application/xml;charset=utf-8,"+escape(e),t.download="metajelo.xml",document.body.appendChild(t),t.click(),document.body.removeChild(t)},document.getElementById("metajelo_upload").addEventListener("change",handleFileSelect,!1),document.getElementById("metajelo_xml_entry").defaultValue=document.getElementById("autogen_mj_xml").innerText;
},{}]},{},["lgAh"], null)
//# sourceMappingURL=demo.64813bc2.js.map