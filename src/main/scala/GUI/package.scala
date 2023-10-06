import scalafx.application.JFXApp3
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.{GridPane, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.control.ComboBox
import scalafx.Includes._
import scalafx.stage.{DirectoryChooser, FileChooser}
import scalafx.geometry.{Insets, Pos}
import rocFB.rocFB
import rocPD.rocPD
import FileIO.{readFile, writeFile}
import scalafx.scene.paint.Color._
import scalafx.scene.paint._
import scalafx.scene.text.Text



object GUI extends JFXApp3 {
  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "REPARTICION OPTIMA DE CUPOS"

      scene = new Scene(width = 400,height = 600) {
        fill = Color.rgb(236, 215, 198)

        resizable = false;

        val txt = new Text {
          text = "ROC"
          style = "-fx-font: normal bold 50pt sans-serif"
          fill = new LinearGradient(
            endX = 0,
            stops = Stops(Red, DarkRed))
        }

        val algorithmTypeLabel = new Label("Seleccione el tipo de algoritmo");
        val comboBoxItems = List("Fuerza Bruta", "Voraz", "Dinamico");
        val algorithmTypeComboBox = new ComboBox[String](comboBoxItems);
        algorithmTypeComboBox.selectionModel().select(0);

        val inputFileLabel = new Label("Seleccione un archivo de entrada");
        val inputFileButton = new Button("Seleccionar");
        val selectedInputFileLabel = new Label("Ninguno");

        val outputDirLabel = new Label("Directorio de salidas");
        val outputDirButton = new Button("Seleccionar");
        val selectedOutputDirLabel = new Label("Ninguno");

        val executeProgramButton = new Button("Ejecutar");

        executeProgramButton.disable = true;

        val executionResultLabel = new Label("");

        val txtVBox = new VBox(5, txt);

        val strategyPickVBox = new VBox(5, algorithmTypeLabel, algorithmTypeComboBox);

        val inputVBox = new VBox(5, inputFileLabel, inputFileButton,
          selectedInputFileLabel);

        val outputVBox = new VBox(5, outputDirLabel, outputDirButton,
          selectedOutputDirLabel);

        val executionVBox = new VBox(5, executeProgramButton, executionResultLabel);




        inputFileButton.onAction = (e: ActionEvent) => {
          val fileChooser = new FileChooser;
          var selectedFile = fileChooser.showOpenDialog(stage)+ "";
          if (selectedFile == "null") {
            selectedFile = "Ninguno";
            executeProgramButton.disable = true;
          } else {
            val outputPath = selectedOutputDirLabel.text();
            if (outputPath != "Ninguno") executeProgramButton.disable = false;
          };

          selectedInputFileLabel.text = selectedFile;

        }

        outputDirButton.onAction = (e:ActionEvent) => {
          val dirChooser = new DirectoryChooser;
          var selectedFile = dirChooser.showDialog(stage)+"";
          if (selectedFile == "null") {
            selectedFile = "Ninguno";
            executeProgramButton.disable = true;
          } else {
            val inputPath = selectedInputFileLabel.text();
            if (inputPath != "Ninguno") executeProgramButton.disable = false;
          }

          selectedOutputDirLabel.text = selectedFile;
        }

        executeProgramButton.onAction = (e: ActionEvent) => {

          try {

            val selectedStrategy = algorithmTypeComboBox.selectionModel.apply.getSelectedIndex();

            val inputPath = selectedInputFileLabel.text();

            val e = readFile(inputPath)

            val fileName = inputPath.substring(inputPath.lastIndexOf("\\")+1)

            var strategyPrefix = "";

            val sol = selectedStrategy match {
              case 0 => {strategyPrefix="fb";rocFB(e._1,e._2,e._3,e._4)}
              case 2 => {strategyPrefix="pd";rocPD(e._1,e._2,e._3,e._4)}
              case _ => {strategyPrefix="pd";rocPD(e._1,e._2,e._3,e._4)} // Placeholder
            }

            executionResultLabel.text = "Procesando...";

            writeFile(sol, selectedOutputDirLabel.text(), strategyPrefix + "_" + fileName);

            executionResultLabel.text = "Ejecutado exitosamente";

          } catch {
            case e => println(e);executionResultLabel.text = "Ha ocurrido un error";
          }

        }

        txtVBox.layoutX = 80;
        txtVBox.layoutY = 20;

        strategyPickVBox.layoutX = 80;
        strategyPickVBox.layoutY = 110;

        inputVBox.layoutX = 80;
        inputVBox.layoutY = 200;

        outputVBox.layoutX = 80;
        outputVBox.layoutY = 300;

        executionVBox.layoutX = 80;
        executionVBox.layoutY = 420;

        content = List(txtVBox, strategyPickVBox, inputVBox, outputVBox, executionVBox);

      }
    }

  }
}