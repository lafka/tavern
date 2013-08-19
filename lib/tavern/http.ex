defmodule Tavern.HTTP do

  @moduledoc """
    HTTP helper functions functions
  """

  def status("Continue") do                        100 end
  def status("Switching Protocols") do             101 end
  def status("Processing") do                      102 end
  def status("OK") do                              200 end
  def status("Created") do                         201 end
  def status("Accepted") do                        202 end
  def status("Non-Authoritative Information") do   203 end
  def status("No Content") do                      204 end
  def status("Reset Content") do                   205 end
  def status("Partial Content") do                 206 end
  def status("Multi-Status") do                    207 end
  def status("IM Used") do                         226 end
  def status("Multiple Choices") do                300 end
  def status("Moved Permanently") do               301 end
  def status("Found") do                           302 end
  def status("See Other") do                       303 end
  def status("Not Modified") do                    304 end
  def status("Use Proxy") do                       305 end
  def status("Switch Proxy") do                    306 end
  def status("Temporary Redirect") do              307 end
  def status("Bad Request") do                     400 end
  def status("Unauthorized") do                    401 end
  def status("Payment Required") do                402 end
  def status("Forbidden") do                       403 end
  def status("Not Found") do                       404 end
  def status("Method Not Allowed") do              405 end
  def status("Not Acceptable") do                  406 end
  def status("Proxy Authentication Required") do   407 end
  def status("Request Timeout") do                 408 end
  def status("Conflict") do                        409 end
  def status("Gone") do                            410 end
  def status("Length Required") do                 411 end
  def status("Precondition Failed") do             412 end
  def status("Request Entity Too Large") do        413 end
  def status("Request-URI Too Long") do            414 end
  def status("Unsupported Media Type") do          415 end
  def status("Requested Range Not Satisfiable") do 416 end
  def status("Expectation Failed") do              417 end
  def status("I'm a teapot") do                    418 end
  def status("Unprocessable Entity") do            422 end
  def status("Locked") do                          423 end
  def status("Failed Dependency") do               424 end
  def status("Unordered Collection") do            425 end
  def status("Upgrade Required") do                426 end
  def status("Precondition Required") do           428 end
  def status("Too Many Requests") do               429 end
  def status("Request Header Fields Too Large") do 431 end
  def status("Internal Server Error") do           500 end
  def status("Not Implemented") do                 501 end
  def status("Bad Gateway") do                     502 end
  def status("Service Unavailable") do             503 end
  def status("Gateway Timeout") do                 504 end
  def status("HTTP Version Not Supported") do      505 end
  def status("Variant Also Negotiates") do         506 end
  def status("Insufficient Storage") do            507 end
  def status("Not Extended") do                    510 end
  def status("Network Authentication Required") do 511 end

 end
