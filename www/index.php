
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<! --- R-Forge Logo --- >
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> 
<hr>
<h1> <span style="color: rgb(0, 0, 0);"> <a name="top"> </a><big style="color: rgb(0, 0, 153);"><span style="color: rgb(51, 102, 255);"><span><a href="http://www.r-project.org/"><img style="border: 0px solid ; width: 152px; height: 101px;" alt="R-Logo" src="RLOGO.jpg"></a></span></span></big>

R-Package
<span style="color: rgb(51, 102, 255);">"robKalman"&nbsp;&nbsp; <span></span>
</h1>
<h1> </h1>
</div>
<hr style="width: 100%; height: 2px;">
<div style="text-align: justify;"> Version: 0.2&nbsp;&nbsp; --- still
in development stage<br>
Release Date: 10-09-08<br>
Authors: <a href="mailto:peter.ruckdeschel@itwm.fraunhofer.de?subject=%5Bproject%5D">Peter
Ruckdeschel</a>, <a href="mailto:bernhard.spangl@boku.ac.at">Bernhard Spangl</a><br>

Required R-Version:
<ul>
  <li>&gt;=2.3.0 for version 0.2,</li>
</ul>
Dependencies: requires packages <br>
<ul>
  <li>"<span style="color: rgb(51, 102, 255);">startupmsg"&nbsp;</span></li>
  <li>"<span style="color: rgb(51, 102, 255);">MASS</span>"</li>

  <li>"<span style="color: rgb(51, 102, 255);">limma</span>"</li>
  <li>"<span style="color: rgb(51, 102, 255);">dse1</span>"</li>
  <li>"<span style="color: rgb(51, 102, 255);">dse2</span>"</li>
  <li>"<span style="color: rgb(51, 102, 255);">robustbase</span>"</li>

</ul>
all available from <a href="http://cran.r-project.org/mirrors.html">CRAN</a>;
</div>
<hr style="width: 100%; height: 2px;">
<h2>What is "robKalman" meant for?</h2>
<div style="text-align: justify; color: rgb(0, 0, 0);"> The aim of
package "<span style="color: rgb(51, 102, 255);">robKalman</span>" is
to
provide routines for robust Kalman Filtering.<br>
In particular the rLS- and ACM-filter are available.<br>
</div>
<br>

</div>
<hr style="width: 100%; height: 2px;"><br>
<h2>Manual/Documentation</h2>
<div style="text-align: justify; color: rgb(0, 0, 0);">
Here will be a vignette soon...<br>
For the moment here are some <a href="robKalman_slides_Wien.pdf">slides</a> 
(<a href="robKalman_slides_Wien_ho.pdf">handout-version</a>) and a
<a href="robKalman_poster_Wien.pdf">poster</a> presented at 
<a href="http://www.r-project.org/useR-2006/">UseR 2006</a>, and some 
<a href="robKalman_slides_Banff.pdf">slides</a> from the <a href="http://www.birs.ca/birspages.php?task=displayevent&event_id=07w5064">Banff RsR workshop</a> 2007.<br>

</div>
<br>
<hr style="width: 100%; height: 2px;">
<h2>License</h2>
<div style="text-align: justify;"><span style="color: rgb(0, 0, 0);">
This software is distributed under the terms of the GNU GENERAL</span>
<br style="color: rgb(0, 0, 0);">
<span style="color: rgb(0, 0, 0);">PUBLIC LICENSE Version 2, June 1991,
confer</span> <a href="http://www.gnu.org/copyleft/gpl.html">http://www.gnu.org/copyleft/gpl.html</a><br>
</div>
<br>
<hr style="width: 100%; height: 2px;">
<h2>Download</h2>

<h3 style="text-align: justify; color: rgb(0, 0, 0);">Windows</h3>
<ul style="text-align: justify; color: rgb(0, 0, 0);">
  <li>as <a href="robKalman_0.2.zip">Win-Zip-File for R &gt;=
2.3.0&nbsp;</a>
(Version 0.2)</li>
</ul>
<ul style="text-align: justify; color: rgb(0, 0, 0);">
  <li>to be installed by
    <ul>
      <li>the R-gui File picker [Packages -&gt;Install package(s) from
local zip-files...] or <br>

      </li>
      <li style="color: rgb(51, 102, 255);">
        <pre>install.packages("robKalman", &lt;path to the file distr.zip in your File system&gt;, NULL)</pre>
      </li>
    </ul>
  </li>
  <li>to be removed by <br>

    <pre style="color: rgb(51, 102, 255);" wrap="">remove.packages("robKalman")</pre>
  </li>
  <li>to be used by <br>
    <pre><span style="color: rgb(51, 102, 255);">library("robKalman")</span> <br></pre>
or
    <pre><span style="color: rgb(51, 102, 255);">require("robKalman")</span> <br></pre>
  </li>

</ul>
<h3 style="text-align: justify; color: rgb(0, 0, 0);">Linux</h3>
<ul style="text-align: justify; color: rgb(0, 0, 0);">
  <li>as <a href="robKalman_0.2.tar.gz">Linux-tar-gz-File for R
&gt;= 2.3.0&nbsp;</a> (Version 0.2)</li>
</ul>
<ul style="text-align: justify; color: rgb(0, 0, 0);">
  <li>to be installed by<br>
    <pre><span style="color: rgb(51, 102, 255);">R CMD INSTALL robKalman_0.2.tar.gz</span><br></pre>

  </li>
  <li>to be removed by<br>
    <pre style="color: rgb(51, 102, 255);" wrap="">R CMD REMOVE robKalman</pre>
  </li>
  <li>to be used as under Windows </li>
</ul>
<br>
<hr style="width: 100%; height: 2px; margin-left: 0px; margin-right: 0px; color: rgb(0, 0, 0);">
<h2 style="text-align: center; color: rgb(0, 0, 153);">Demos</h2>

<span style="color: rgb(0, 0, 0);">also see</span> <span style="color: rgb(51, 102, 255);">demo(package="robKalman")</span> <span style="color: rgb(0, 0, 0);"></span><br>
<br style="color: rgb(0, 0, 0);">
<hr style="width: 100%; height: 2px; color: rgb(0, 0, 0);">
<h2 style="text-align: center; color: rgb(0, 0, 153);">Version history:</h2>
<h3 style="text-align: left; color: rgb(0, 0, 0);">Version
0.2&nbsp; (10-09-08)</h3>
<ul>
  <li>integration of S4 layer; also see <span style="color: rgb(51, 102, 255);">NEWS("robKalman")</span></li>
</ul>
<h3 style="text-align: left; color: rgb(0, 0, 0);">First version
(0.1)&nbsp; (06-12-06)</h3>
<ul>
  <li>first developper version</li>
</ul>
<div style="text-align: left; color: rgb(0, 0, 0);">
<hr style="width: 100%; height: 2px; color: rgb(0, 0, 0);">
<h2 style="text-align: center; color: rgb(0, 0, 153);">Our plans for
the next version:</h2>
<ul>
  <li><span style="color: rgb(51, 102, 255);"><br>
    </span></li>
</ul>
<br style="color: rgb(0, 0, 0);">
<hr style="width: 100%; height: 2px; color: rgb(0, 0, 0);">
<h2 style="text-align: center; color: rgb(0, 0, 153);">Things we invite
other people to do</h2>
<ul>
  <li><br>

  </li>
</ul>
<p></p>
<p></p>
If you want to <b>collaborate</b> (which you are welcome to do!):<br>
<ul>
<li> please read <a
href="http://robkalman.r-forge.r-project.org/HOWTO-collaborate.txt">HOWTO-collaborate.txt</a>
 [a short HOWTO for svn and R-Forge in 10]<br>
</a></li>
</ul>
<br>
</div>
<br><br>
<ul>
  <span style="color: rgb(51, 102, 255);"></span>
</ul>

<ul>
  <span style="color: rgb(51, 102, 255);"></span>
</ul>
</div>
<hr style="width: 100%; height: 2px; color: rgb(0, 0, 0);">
<div style="text-align: justify; color: rgb(0, 0, 0);">This page is
maintained by <a href="mailto:peter.ruckdeschel@itwm.fraunhofer.de?subject=robkalman-package">Peter
Ruckdeschel</a> and last updated on 28-08-08.<br>
</div>
<br>
<br>
</p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
