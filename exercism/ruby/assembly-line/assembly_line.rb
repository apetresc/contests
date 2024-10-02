class AssemblyLine
  SPEED_PER_HOUR = 221
  SUCCESS_RATES = [1, 1, 1, 1, 1, 0.9, 0.9, 0.9, 0.9, 0.8, 0.77]

  def initialize(speed)
    raise "Invalid speed" unless 1 <= speed and speed <= 10
    @speed = speed
  end

  def production_rate_per_hour
    @speed * SPEED_PER_HOUR * SUCCESS_RATES[@speed]
  end

  def working_items_per_minute
    (production_rate_per_hour / 60).floor
  end
end
