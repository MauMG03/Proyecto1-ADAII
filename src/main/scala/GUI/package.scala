import scalafx.application.JFXApp3
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.VBox
import scalafx.scene.control.ComboBox
import scalafx.Includes._
import scalafx.stage.{FileChooser, DirectoryChooser}

import rocFB.rocFB
import rocPD.rocPD

import FileIO.{writeFile, readFile}

import java.nio.file.{Paths, Path}



object GUI extends JFXApp3 {
  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "REPARTICION OPTIMA DE CUPOS"
      scene = new Scene(800, 600) {


        val algorithmTypeLabel = new Label("Seleccione el tipo de algoritmo");
        val comboBoxItems = List("Fuerza Bruta", "Voraz", "Dinamico");
        val algorithmTypeComboBox = new ComboBox[String](comboBoxItems);
        algorithmTypeComboBox.selectionModel().select(0);

        val inputFileLabel = new Label("Seleccione un archivo de entrada");
        val inputFileButton = new Button("Seleccionar");
        val selectedInputFileLabel = new Label("Ninguno");

        val outputDirLabel = new Label("Seleccione un directorio donde se crearan las salidas");
        val outputDirButton = new Button("Seleccionar");
        val selectedOutputDirLabel = new Label("Ninguno");

        val executeProgramButton = new Button("Ejecutar");

        executeProgramButton.disable = true;

        val executionResultLabel = new Label("");

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

            val fileName = inputPath.split("/").last;

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
            case _ => executionResultLabel.text = "Ha ocurrido un error";
          }

        }

        content = new VBox(algorithmTypeLabel, algorithmTypeComboBox, inputFileLabel, inputFileButton,
          selectedInputFileLabel, outputDirLabel, outputDirButton, selectedOutputDirLabel, executeProgramButton,
          executionResultLabel);

      }
    }

  }
}