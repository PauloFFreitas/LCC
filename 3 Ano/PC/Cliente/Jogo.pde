import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.ArrayList;
import java.util.HashMap;
import processing.core.PApplet;

class Game {

  ArrayList<Astronaut> players;
  ArrayList<Planet> planets;
  ArrayList<String> playerNames;
  ArrayList<Integer> playerColors;
  Lock l;



  public Game (ArrayList<Astronaut> players, ArrayList<Planet> planets) {

      this.players  = players;
      this.planets = planets;
      this.l = new ReentrantLock();
  }

  void update (ArrayList<Astronaut> players, ArrayList<Planet> planets) {

    this.l.lock();
    //println("Teste UPDATE");
    try {
      this.players  = players;
      this.planets = planets;
    }finally{
      this.l.unlock();
    }
  }


   void draw(PApplet appc) {
    
    // Desenha o sol
    appc.fill(255, 255, 0); // amarelo
    appc.ellipse(width/2, height/2, 50*2, 50*2); // sol
     
    for (Planet p : this.planets) {
        p.display(appc);
    }
    int margin = 35; // Margem a partir da borda
    int lineHeight = 35; // Altura da linha
    int y = margin; // Iniciar a partir da margem superior

    for (Astronaut a : this.players) {
        appc.fill(255,255,255);
        appc.textSize(16);
        appc.text(a.name, (a.x) - 10, (a.y)-15);
        
        appc.fill(255,255,255);
        appc.textSize(30);
        appc.text("Boost - " + a.name + " : " + a.boost, margin, y);
        y += lineHeight; // Incrementar a posição vertical para a próxima linha
        a.display(appc);
      }
  }

  
}
