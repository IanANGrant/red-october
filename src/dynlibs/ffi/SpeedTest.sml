fun timethis thunk =
    let fun usecs {sys : Time.time ,usr : Time.time} =
                  Time.toMicroseconds usr
        val timer = Timer.startCPUTimer()
        val a = thunk()
        val interval = Timer.checkCPUTimes timer
    in usecs (#nongc (interval))
    end
