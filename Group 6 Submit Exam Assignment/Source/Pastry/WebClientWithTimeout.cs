using System;
using System.Net;

// From https://stackoverflow.com/questions/295557/c-downloading-a-url-with-timeout
namespace NewWebClient {
    public class WebClientWithTimeout : WebClient
    {
        private readonly int timeoutMilliseconds;
        public WebClientWithTimeout(int timeoutMilliseconds)
        {
            this.timeoutMilliseconds = timeoutMilliseconds;
        }

        protected override WebRequest GetWebRequest(Uri address)
        {
            var result = base.GetWebRequest(address);
            result.Timeout = timeoutMilliseconds;
            return result;
        }
    }
}
// Sample usage:
//
// public string GetRequest(Uri uri, int timeoutMilliseconds)
// {
//   using (var client = new WebClientWithTimeout(timeoutMilliseconds))
//   {
//     return client.DownloadString();
//   }
// }