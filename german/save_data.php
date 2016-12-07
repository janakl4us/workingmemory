<?php
    // the $_POST[] array will contain the passed in filename and data
    // the directory "data" is writable by the server (chmod 777)
    // the file path needs to be adjusted accordingly!
    $filename = "data/".$_POST['filename'];
    $data = $_POST['filedata'];
    // write the file to disk
    file_put_contents($filename, $data);
?>
