defmodule Tavern.Middleware do

  require :cowboy_req, as: Req

  @moduledoc """
    Collection of convenience middlewares
  """

  defmodule Log do
    def execute(req, env) do
      {method, req} = Req.method req
      {url, req}    = Req.url req

      IO.puts "#{get_now} #{method} #{url}"

      {:ok, req, env}
    end

    defp get_now do
      {{y,m,d},{h,i,s}} = now = :calendar.local_time
      now_utc = :lists.last(:calendar.local_time_to_universal_time_dst(now))
      local = :calendar.datetime_to_gregorian_seconds now
      utc   = :calendar.datetime_to_gregorian_seconds now_utc

      tz = div(local - utc, 60);
      {tzh, tzm} = {div(tz, 60), rem(tz, 60)}

      tzsign = if tzh < 0 do "" else "+" end

      fmt = "~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b~s~2..0b:~2..0b"
      :lists.flatten(:io_lib.format fmt, [y, m, d, h, i, s, tzsign, tzh, tzm])
    end
  end

  defmodule CORS do

    @moduledoc """
      Add correct CORS headers for a request

      There is no special handling the preflight "OPTIONS" method here
      as that code can be found in @link Tavern.Invoker
    """

    def execute(req, env) do
      host = case Req.header "origin", req do
        {:undefined, req} -> "*"
        {host, req} -> host end

      req = Req.set_resp_header "Access-Control-Allow-Origin", host, req
      req = Req.set_resp_header "Access-Control-Allow-Methods", "GET, PUT, POST, DELETE, PATCH", req
      req = Req.set_resp_header "Access-Control-Allow-Headers", "X-Requested-With,Origin,Content-Type,Accept,Cookie", req
      req = Req.set_resp_header "Access-Control-Allow-Credentials", "true", req
      req = Req.set_resp_header "Access-Control-Max-Age", "1800", req

      {:ok, req, env}
    end
  end
end
