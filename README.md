# reqT Webapplication

In the Course ETS170 Requirement Engineering at Lund University, the Requirement Engineering tool reqT has been used to model requirements but it ahs been identified that the tool in its current form is difficult to use for students with limited programming experience. To solve this problem, this application has been developed in order to appeal to both developers and non-developers. The application has primarily been built using Scala.js and React. 

# Developer guide
In order to run this application one is required to download SBT and JVM 8. 

<ol>
  <li> Download the repository using:</li>
  
      
      git clone
      

  <li> Locate the repository folder in your terminal.</li>

  <li>(optional) If you want to change the host location, the Config class contains the url and port for the server. Change these to update the server location. i.e, if you want to run the application locally update the hostname to localhost</li>
  <li> write the following in the terminal</li>
  
      sbt run

   
  <li>SBT will now download all the required dependencies, while may take a while. </li>
  <li>When the application has started, open a web browser and navigate to <i>hostname</i>:<i>port</i> </li>
</ol>

# User guide
## Add or Move Elements
Add elements by finding them in the list and then <b>dragging</b> them into place in the model. The position of the element is indicated by the temporary node.
It is not possible to add children to attributes, and this is indicated by a red border.
The elements can be removed by pressing the <b>Delete</b>-button, the button with a cross.

### Short commands
<ul>
  <li> Click on a node to select it. When a node is selected it will have a black border.
  <li><b>Ctrl+X</b>, <b>Ctrl+C</b> and <b>Ctrl + V</b> will cut, copy, and paste the selected element of the tree.</li> 
  <li> Pressing <b>Delete</b>-key will delete selected element and its children. </li>
  <li> Holding <b>Ctrl</b> while dragging and dropping an element or submodel will copy instead of moving it.
</ul>

<br>
  <img height = "500" src = "https://github.com/NilssonJohan/reqT.js/blob/master/screenshots/dragging.png">
</br>


## Update Elements
<b> Double-click </b> on the element you want to update. A window like the one in the picture below will appear, do the changes and press <b> Save Changes </b>. Relation types can be changed by just selecting a new type from the drop-down menu next to the element node in the model.

<br>
  <img height = "300" src = "https://github.com/NilssonJohan/reqT.js/blob/master/screenshots/addModal.png">
</br>

## Create new Models
Press the add button, the button with a green plus and then chose if you want to create a new empty model by pressing <b>add empty model</b> or one that is a clone of the current model by pressing <b>add current model</b>. All models can be found in the list and to switch between models just click the tabs. Models can be deleted by pressing the <b>Delete</b>-button, the button with a cross on.

<br>
  <img height = "80" src = "https://github.com/NilssonJohan/reqT.js/blob/master/screenshots/current.png">
</br>

## Import and Export Models
To import models saved in text files press the <b>Import</b>-button and select which file you want to open. The file will be opened in a new model. 

To Export models, press the <b>Export</b>-button and it will automatically be downloaded in a text file.

## Templates

To open a template model press the <b>Templates</b> menu in the header, click on the desired template and reqT will let you open it in a new model.

## 100 Dollar Method
To do a 100 dollar prioritization first construct the model you want to conduct it on or choose the <i>Prioritization $100 method</i> template, and then press the <b>100$</b>-button in the header and specify on what entities the method should be carried out. When you press the <b>OK</b>-button, reqT will let you save the result as a new model.


<br>
  <img height = "300" src = "https://github.com/NilssonJohan/reqT.js/blob/master/screenshots/100dollar.png">
</br>


<br>
  <img height = "350" src = "https://github.com/NilssonJohan/reqT.js/blob/master/screenshots/fetchModal.png">
</br>

## Ordinal Ranking
First, construct the model you want to rank or choose the <i>Prioritization Ordinal Ranking</i> template. Then, press the <b>Ordinal Ranking</b>-button and choose on which entity to perform the ranking. reqT will let you rank the entities in pairs and set a deviation number. Pick the one with the highest priority in each pair by clicking on it. The prioritized entity are highlighted with a green background. Press <b>OK</b> and reqT will let you save the result as a new model.

<br>
  <img height = "400" src = "https://github.com/NilssonJohan/reqT.js/blob/master/screenshots/Ordinal.png">
</br>

## Release Planning

To do a release planning on a model, it must follow the particular pattern. Models following this pattern can be found in templates <i>Release planning I</i> and <i>Release planning II</i>. When an appropriate model is active, you can press the <b>Release</b>-button and specify how to perform the method. Then press <b>OK</b>-button and reqT will let you save the result as a new model.

<br>
  <img height = "300" src = "https://github.com/NilssonJohan/reqT.js/blob/master/screenshots/releaseplan.png">
</br>

## Element List
A searchable and filterable list containing all reqT Elements. To search just write what you're looking for in the search box. To filter either check the filter boxes or type <i>entity</i> or <i>attribute</i> in the search box. Drag these elements into the tree when you want to add something to your model.
<br>
  <img height = "300" src = "https://github.com/NilssonJohan/reqT.js/blob/master/screenshots/element.png">
</br>


## reqT Terminal
The terminal can be used to interact with the reqT process. The <b>send model</b>-button will send the current model to the process; the model will be accessible by a variable called resX, where X is a number. There is also a <b>Get</b>-method that can be used to fetch and save models. This means that you can manipulate the model using the terminal. You will have to fetch the model from the process by using the <b>Get</b>-method, and if you want to work on a model from the GUI, you first have to send it to the process.
The <b>restart reqT</b>-button will restart the connection to the server if it disconnects.

<br>
  <img height = "300" src = "https://github.com/NilssonJohan/reqT.js/blob/master/screenshots/getsend.png">
</br>

***

# Start the server

To start the server you will need two passwords, to ssh into the virtual machine and the sudo password.

<ol>
  <li>To login in to the virtual machine write the following in a terminal. </li>

```
ssh -X reqtuser@vm45.cs.lth.se 
```

<li>Then write the following in the terminal. </li>

```
cd Desktop/reqT.js
su
sbt run
```
<li>The server is now running and you can start reqT.js at the URL <a href="http://vm45.cs.lth.se:9000/">http://vm45.cs.lth.se:9000/</a> </li> 

To change the host location, the config class has to be updated accordingly.


## Run the application
<ol>
  <li> Open a web browser (Preferably Google Chrome).</li>
  <li> Open <a href="http://vm45.cs.lth.se:9000/">http://vm45.cs.lth.se:9000/</a> </li> 
</ol>



