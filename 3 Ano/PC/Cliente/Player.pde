public class Player {
    private String username;
    private int victories;
    private int defeats;
    private int level;

    // Construtor
    public Player(String username, int victories, int defeats,int level) {
        this.username = username;
        this.level = level;
        this.victories = victories;
        this.defeats = defeats;
    }

    // Métodos getters
    public String getUsername() {
        return username;
    }

    public int getLevel() {
        return level;
    }
    public int getVictories() {
      return victories;
    }
    public int getDefeats() {
      return defeats;
    }
}
