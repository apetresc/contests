class Clock {
    private static final int MINUTES_IN_DAY = 24 * 60;
    private int totalMinutes = 0;

    Clock(int hours, int minutes) {
        add(hours * 60 + minutes);
    }

    void add(int minutes) {
        totalMinutes += minutes % (MINUTES_IN_DAY);
        while (totalMinutes < 0) {
            totalMinutes += MINUTES_IN_DAY;
        }
    }

    @Override
    public String toString() {
        return String.format("%02d:%02d", (totalMinutes / 60) % 24, totalMinutes % 60);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Clock) {
            return totalMinutes == ((Clock) obj).totalMinutes;
        } else {
            return false;
        }
    }
}